# Phase 2.6: Argon2id Indexing Functions - Comprehensive Research

**Date**: 2025-10-17
**Status**: Research Complete - Ready for Implementation
**Target**: 40/40 VCs (100% proof rate)

---

## EXECUTIVE SUMMARY

This document provides exhaustive research and implementation strategy for **Argon2id indexing functions** (RFC 9106 Sections 3.3-3.4). The indexing mechanism determines which memory blocks are read during the memory-hard computation, implementing either:

1. **Argon2i**: Data-independent indexing (side-channel resistant)
2. **Argon2d**: Data-dependent indexing (GPU-resistant)
3. **Argon2id**: Hybrid approach (first half Argon2i, second half Argon2d)

**Key Finding**: The indexing algorithm is **complex but fully deterministic** and can be verified in SPARK with careful contract design and bounded arithmetic proofs.

---

## 1. ALGORITHM ANALYSIS (RFC 9106 SECTIONS 3.3-3.4)

### 1.1 Memory Organization

Argon2 organizes memory into a hierarchical structure:

```
Memory = Array[0..p-1] of Lane        -- p lanes (parallelism)
Lane   = Array[0..q-1] of Block       -- q blocks per lane
Block  = Array[0..127] of U64         -- 128 x 64-bit words

where:
  p = parallelism degree (SparkPass: p=1)
  q = blocks per lane = m' / p
  m' = total memory blocks (SparkPass: 131,072 for 1 GiB)
```

Each lane is further divided into **4 segments** (sync points):

```
Segment_Length = q / 4
Segment_Index ∈ [0, 3]
```

**RFC 9106 Section 3.3**:
> "The j-th slice in the i-th lane contains blocks with indices from
> i·q + j·(q/4) to i·q + (j+1)·(q/4) - 1"

### 1.2 Argon2i: Data-Independent Indexing

**Purpose**: Generate indices pseudo-randomly without reading block contents (resistant to side-channel attacks).

**RFC 9106 Section 3.4.1.1**:

#### Step 1: Input Block Creation

For each segment, create input block Z:

```
Z = LE64(r) || LE64(l) || LE64(sl) || LE64(m') || LE64(t) || LE64(y)

where:
  r   = pass number (current iteration)
  l   = lane number
  sl  = slice number (segment index)
  m'  = total memory blocks
  t   = total passes
  y   = Argon2 type (0=d, 1=i, 2=id)
```

This fills the first 6 words of a 128-word block. Remaining words are zero.

#### Step 2: Address Block Generation

Generate address blocks using compression function:

```
procedure next_addresses(
   address_block : out Block;
   input_block   : in out Block;
   zero_block    : Block
)
is
begin
   input_block.v[6] := input_block.v[6] + 1;  -- Increment counter
   fill_block(zero_block, input_block, address_block, with_xor => False);
   fill_block(zero_block, address_block, address_block, with_xor => False);
end next_addresses;
```

**Key insight**: Each call produces 128 pseudo-random U64 values. We need 1 value per block, so generate new addresses every 128 blocks.

#### Step 3: Extract J₁ and J₂

```
pseudo_rand := address_block.v[i mod 128]  -- 64-bit value
J₁ := pseudo_rand and 0xFFFFFFFF           -- Lower 32 bits
J₂ := pseudo_rand shr 32                   -- Upper 32 bits
```

### 1.3 Argon2d: Data-Dependent Indexing

**Purpose**: Make indices depend on actual block contents (resistant to GPU/ASIC attacks).

**RFC 9106 Section 3.4.1.2**:

```
pseudo_rand := prev_block.v[0]             -- First word of previous block
J₁ := pseudo_rand and 0xFFFFFFFF           -- Lower 32 bits
J₂ := pseudo_rand shr 32                   -- Upper 32 bits
```

**Critical difference**: Argon2d reads from memory (cache-timing vulnerable), Argon2i does not.

### 1.4 Argon2id: Hybrid Indexing

**RFC 9106 Section 3.4.1.3**:

```ada
function Get_Indexing_Mode (Pass : Pass_Index; Slice : Segment_Index)
   return Indexing_Mode
is
begin
   if Pass = 0 and Slice in 0 .. 1 then
      return Data_Independent;  -- Argon2i for first half of first pass
   else
      return Data_Dependent;    -- Argon2d for everything else
   end if;
end Get_Indexing_Mode;
```

**Rationale**:
- First half: Side-channel resistance (Argon2i)
- Second half: GPU resistance (Argon2d)
- Best of both worlds

### 1.5 Reference Block Calculation (index_alpha)

Once we have J₁ and J₂, calculate the actual block index:

#### Step 1: Determine Reference Lane

```ada
ref_lane := J₂ mod p  -- Which lane to read from

-- Special case: First segment of first pass cannot cross lanes
if Pass = 0 and Slice = 0 then
   ref_lane := current_lane;
end if;
```

#### Step 2: Calculate Reference Area Size

**RFC 9106 Section 3.4.2**:

```ada
if Pass = 0 then
   if Slice = 0 then
      -- Can only reference already-filled blocks in current segment
      reference_area_size := Index - 1;
   else
      -- Can reference all previous segments + current progress
      reference_area_size := Slice * Segment_Length + Index - 1;
   end if;
else
   -- Can reference almost entire lane (except current block)
   if same_lane then
      reference_area_size := Lane_Length - Segment_Length + Index - 1;
   else
      reference_area_size := Lane_Length - Segment_Length - 1;
   end if;
end if;
```

#### Step 3: Map J₁ to Relative Position

**RFC 9106 Section 3.4.2** (mapping function):

```ada
-- Non-uniform distribution favoring recent blocks
x := J₁ mod reference_area_size;        -- Uniform in [0, ref_area)
y := (J₁ * J₁) / 2^32;                  -- Square and shift
z := (reference_area_size * y) / 2^32;  -- Scale to area
relative_position := reference_area_size - 1 - z;
```

**Key property**: This favors recent blocks (locality of reference).

#### Step 4: Calculate Absolute Position

```ada
if Pass = 0 and Slice = 0 then
   start_position := 0;
elsif same_lane then
   start_position := (Slice + 1) * Segment_Length mod Lane_Length;
else
   start_position := Slice * Segment_Length;
end if;

ref_index := (start_position + relative_position) mod Lane_Length;
```

---

## 2. C REFERENCE IMPLEMENTATION ANALYSIS

### 2.1 fill_segment Function (Complete)

```c
void fill_segment(const argon2_instance_t *instance,
                  argon2_position_t position)
{
    block *ref_block = NULL, *curr_block = NULL;
    block address_block, input_block, zero_block;
    uint64_t pseudo_rand, ref_index, ref_lane;
    uint32_t prev_offset, curr_offset;
    uint32_t starting_index;
    uint32_t i;
    int data_independent_addressing;

    // Determine indexing mode
    data_independent_addressing =
        (instance->type == Argon2_i) ||
        (instance->type == Argon2_id && (position.pass == 0) &&
         (position.slice < ARGON2_SYNC_POINTS / 2));

    // Initialize address generation for Argon2i
    if (data_independent_addressing) {
        init_block_value(&zero_block, 0);
        init_block_value(&input_block, 0);

        input_block.v[0] = position.pass;
        input_block.v[1] = position.lane;
        input_block.v[2] = position.slice;
        input_block.v[3] = instance->memory_blocks;
        input_block.v[4] = instance->passes;
        input_block.v[5] = instance->type;
    }

    starting_index = 0;

    if ((0 == position.pass) && (0 == position.slice)) {
        starting_index = 2; // Skip first 2 blocks (already initialized)
        if (data_independent_addressing) {
            next_addresses(&address_block, &input_block, &zero_block);
        }
    }

    // Calculate offsets
    curr_offset = position.lane * instance->lane_length +
                  position.slice * instance->segment_length + starting_index;

    if (0 == curr_offset % instance->lane_length) {
        prev_offset = curr_offset + instance->lane_length - 1;
    } else {
        prev_offset = curr_offset - 1;
    }

    // Main loop: fill each block in segment
    for (i = starting_index; i < instance->segment_length;
         ++i, ++curr_offset, ++prev_offset)
    {
        // Wrap prev_offset if needed
        if (curr_offset % instance->lane_length == 1) {
            prev_offset = curr_offset - 1;
        }

        // Get pseudo-random value
        if (data_independent_addressing) {
            if (i % ARGON2_ADDRESSES_IN_BLOCK == 0) {
                next_addresses(&address_block, &input_block, &zero_block);
            }
            pseudo_rand = address_block.v[i % ARGON2_ADDRESSES_IN_BLOCK];
        } else {
            pseudo_rand = instance->memory[prev_offset].v[0];
        }

        // Calculate reference lane
        ref_lane = ((pseudo_rand >> 32)) % instance->lanes;

        if ((position.pass == 0) && (position.slice == 0)) {
            ref_lane = position.lane;  // Cannot cross lanes in first segment
        }

        // Calculate reference block index
        position.index = i;
        ref_index = index_alpha(instance, &position, pseudo_rand & 0xFFFFFFFF,
                                ref_lane == position.lane);

        // Mix blocks
        ref_block = instance->memory + instance->lane_length * ref_lane + ref_index;
        curr_block = instance->memory + curr_offset;

        if (0 == position.pass) {
            fill_block(instance->memory + prev_offset, ref_block, curr_block, 0);
        } else {
            fill_block(instance->memory + prev_offset, ref_block, curr_block, 1);
        }
    }
}
```

### 2.2 index_alpha Function (Complete)

```c
uint32_t index_alpha(const argon2_instance_t *instance,
                     const argon2_position_t *position,
                     uint32_t pseudo_rand,
                     int same_lane)
{
    uint32_t reference_area_size;
    uint64_t relative_position;
    uint32_t start_position, absolute_position;

    if (0 == position->pass) {
        // First pass
        if (0 == position->slice) {
            // First slice
            reference_area_size = position->index - 1;
        } else {
            if (same_lane) {
                // Same lane: all prior slices + current progress
                reference_area_size = position->slice * instance->segment_length +
                                      position->index - 1;
            } else {
                reference_area_size = position->slice * instance->segment_length +
                                      ((position->index == 0) ? (-1) : 0);
            }
        }
    } else {
        // Second+ pass
        if (same_lane) {
            reference_area_size = instance->lane_length -
                                  instance->segment_length + position->index - 1;
        } else {
            if (position->index == 0) {
                reference_area_size = instance->lane_length -
                                      instance->segment_length - 1;
            } else {
                reference_area_size = instance->lane_length -
                                      instance->segment_length;
            }
        }
    }

    // Mapping pseudo_rand to [0, reference_area_size)
    relative_position = pseudo_rand;
    relative_position = relative_position * relative_position >> 32;
    relative_position = reference_area_size - 1 -
                        (reference_area_size * relative_position >> 32);

    // Calculate starting position
    if (0 == position->pass) {
        start_position = 0;

        if ((0 != position->slice) || (0 != position->index)) {
            start_position = position->slice * instance->segment_length;
        }
    } else {
        start_position = ((position->slice + 1) * instance->segment_length) %
                         instance->lane_length;
    }

    // Calculate absolute position with wraparound
    absolute_position = (start_position + relative_position) %
                        instance->lane_length;

    return absolute_position;
}
```

### 2.3 next_addresses Function

```c
static void next_addresses(block *address_block,
                          block *input_block,
                          const block *zero_block)
{
    input_block->v[6]++;  // Increment counter
    fill_block(zero_block, input_block, address_block, 0);
    fill_block(zero_block, address_block, address_block, 0);
}
```

**Note**: `ARGON2_ADDRESSES_IN_BLOCK = 128` (full block of addresses).

---

## 3. SPARK IMPLEMENTATION STRATEGY

### 3.1 Type System Design

We need bounded types for all indices to eliminate range checks:

```ada
-- Reference area bounds (for VC efficiency)
-- Worst case: entire lane except current segment
subtype Reference_Area_Size_Type is Natural range 0 .. Active_Blocks_Per_Lane;

-- Relative position within reference area
subtype Relative_Position_Type is Natural range 0 .. Active_Blocks_Per_Lane - 1;

-- J1/J2 values (32-bit extracted from 64-bit pseudo_rand)
subtype J1_Type is U32;  -- Lower 32 bits
subtype J2_Type is U32;  -- Upper 32 bits

-- Pseudo-random value (full 64-bit)
subtype Pseudo_Random_Type is U64;
```

### 3.2 Key Functions Required

#### 3.2.1 Extract J1 and J2

```ada
procedure Extract_J1_J2 (
   Pseudo_Rand : U64;
   J1          : out U32;
   J2          : out U32
) with
   Global => null,
   Post   => J1 = U32 (Pseudo_Rand and 16#FFFF_FFFF#) and
             J2 = U32 (Shift_Right (Pseudo_Rand, 32));
```

#### 3.2.2 Calculate Reference Lane

```ada
function Calculate_Ref_Lane (
   J2          : U32;
   Parallelism : Positive;
   Pos         : Position
) return Lane_Index with
   Pre  => Parallelism = Types.Parallelism,
   Post => Calculate_Ref_Lane'Result in Lane_Index and
           (if Pos.Pass = 0 and Pos.Segment = 0 then
              Calculate_Ref_Lane'Result = Pos.Lane);
```

#### 3.2.3 Calculate Reference Area Size

```ada
function Calculate_Reference_Area_Size (
   Pos       : Position;
   Index     : Natural;
   Same_Lane : Boolean
) return Reference_Area_Size_Type with
   Pre  => Index < Active_Blocks_Per_Segment,
   Post => Calculate_Reference_Area_Size'Result > 0 and
           Calculate_Reference_Area_Size'Result <= Active_Blocks_Per_Lane;
```

#### 3.2.4 Map J1 to Relative Position (index_alpha core)

```ada
function Map_J1_To_Position (
   J1                  : U32;
   Reference_Area_Size : Reference_Area_Size_Type
) return Relative_Position_Type with
   Pre  => Reference_Area_Size > 0,
   Post => Map_J1_To_Position'Result < Reference_Area_Size;
```

**Critical verification challenge**: Proving the 64-bit arithmetic stays in bounds:

```ada
-- Step 1: x := (J₁ * J₁) >> 32
-- Need to prove: U64(J1) * U64(J1) fits in U64 (always true: 2^32 * 2^32 = 2^64)

-- Step 2: y := (reference_area_size * x) >> 32
-- Need to prove: reference_area_size * x fits in U64
-- Worst case: 131072 * 2^32 = 2^49 < 2^64 ✓

-- Step 3: z := reference_area_size - 1 - y
-- Need to prove: y <= reference_area_size - 1
-- Guaranteed by division in Step 2 ✓
```

#### 3.2.5 Calculate Absolute Reference Index

```ada
function Calculate_Ref_Index (
   Pos               : Position;
   Index             : Natural;
   Relative_Position : Relative_Position_Type;
   Same_Lane         : Boolean
) return Block_Index with
   Pre  => Index < Active_Blocks_Per_Segment and
           Relative_Position < Active_Blocks_Per_Lane,
   Post => Calculate_Ref_Index'Result in Block_Index;
```

### 3.3 Address Block Generation (Argon2i)

```ada
-- State for address generation
type Address_Generator_State is record
   Input_Block   : Block;
   Address_Block : Block;
   Counter       : Block_Word_Index;  -- Tracks position in address_block
end record;

procedure Initialize_Address_Generator (
   State : out Address_Generator_State;
   Pos   : Position
) with
   Post => State.Counter = 0 and
           State.Input_Block(0) = U64(Pos.Pass) and
           State.Input_Block(1) = U64(Pos.Lane) and
           State.Input_Block(2) = U64(Pos.Segment) and
           State.Input_Block(3) = U64(Active_Total_Blocks) and
           State.Input_Block(4) = U64(Iterations) and
           State.Input_Block(5) = 2;  -- Argon2id type

procedure Get_Next_Pseudo_Rand (
   State       : in out Address_Generator_State;
   Pseudo_Rand : out U64
) with
   Pre  => State.Counter in Block_Word_Index,
   Post => Pseudo_Rand = State.Address_Block(State.Counter'Old) and
           State.Counter = (State.Counter'Old + 1) mod 128;
```

**Note**: We need to call `next_addresses` when `State.Counter = 0`.

---

## 4. VERIFICATION STRATEGY

### 4.1 Critical Contracts

#### Preconditions

```ada
-- For all indexing functions
Pre => Index < Active_Blocks_Per_Segment and
       Pos.Pass in Pass_Index and
       Pos.Segment in Segment_Index and
       Pos.Lane in Lane_Index and
       Pos.Index = Index
```

#### Postconditions

```ada
-- ref_index must be valid block index
Post => ref_index in Block_Index and

        -- Must not reference current block
        ref_index /= (Pos.Segment * Active_Blocks_Per_Segment + Index) and

        -- First segment of first pass: only reference earlier blocks
        (if Pos.Pass = 0 and Pos.Segment = 0 then
           ref_index < Index);
```

### 4.2 Loop Invariants (fill_segment)

```ada
for Index in Starting_Index .. Active_Blocks_Per_Segment - 1 loop

   pragma Loop_Invariant (Index >= Starting_Index);
   pragma Loop_Invariant (Index < Active_Blocks_Per_Segment);
   pragma Loop_Invariant (Pos.Pass in Pass_Index);
   pragma Loop_Invariant (Pos.Segment in Segment_Index);

   -- All computed indices remain valid
   pragma Loop_Invariant (ref_index in Block_Index);
   pragma Loop_Invariant (ref_lane in Lane_Index);

end loop;
```

### 4.3 Arithmetic Overflow Prevention

**Challenge**: The mapping function requires 64-bit arithmetic that could overflow.

**Solution 1**: Use U64_Mod (modular arithmetic)

```ada
function Map_J1_To_Position_Mod (
   J1                  : U32;
   Reference_Area_Size : Reference_Area_Size_Type
) return Relative_Position_Type
is
   X, Y, Z : U64_Mod;
begin
   X := U64_Mod(J1);
   X := (X * X) / 2**32;  -- Modular division, no overflow
   Y := U64_Mod(Reference_Area_Size) * X;
   Z := Y / 2**32;
   return Relative_Position_Type(Reference_Area_Size - 1 - U64(Z));
end Map_J1_To_Position_Mod;
```

**Solution 2**: Prove bounds explicitly

```ada
pragma Assert (U64(J1) <= U64(U32'Last));
pragma Assert (U64(J1) * U64(J1) <= U64'Last);  -- 2^32 * 2^32 = 2^64
pragma Assert (U64(Reference_Area_Size) <= U64(Active_Blocks_Per_Lane));
pragma Assert (U64(Reference_Area_Size) * X <= U64'Last);  -- Needs proof
```

### 4.4 Ghost Functions

```ada
-- Specification-level function (not executable)
function Ref_Index_Valid (
   Ref_Index : Block_Index;
   Pos       : Position;
   Index     : Natural;
   Same_Lane : Boolean
) return Boolean is
   (Ref_Index /= (Pos.Segment * Active_Blocks_Per_Segment + Index) and
    (if Pos.Pass = 0 and Pos.Segment = 0 then Ref_Index < Index))
with Ghost;
```

---

## 5. FILE STRUCTURE

### 5.1 Proposed Organization

```
sparkpass-crypto-argon2id-index.ads    -- Public interface
sparkpass-crypto-argon2id-index.adb    -- Implementation
```

**Alternative** (if too large):

```
sparkpass-crypto-argon2id-index-types.ads   -- Types and constants
sparkpass-crypto-argon2id-index-i.ads/adb   -- Argon2i (data-independent)
sparkpass-crypto-argon2id-index-d.ads/adb   -- Argon2d (data-dependent)
sparkpass-crypto-argon2id-index.ads/adb     -- Main interface
```

**Recommendation**: Start with single file, split only if VCs exceed 50.

### 5.2 Dependencies

```
sparkpass-crypto-argon2id-index
  ├─ depends on: sparkpass-crypto-argon2id-types
  ├─ depends on: sparkpass-crypto-argon2id-mix (for next_addresses)
  └─ used by: sparkpass-crypto-argon2id-fill (Phase 2.7)
```

---

## 6. DETAILED SPARK SPECIFICATIONS

See next section for complete `.ads` file.

---

## 7. VERIFICATION CHALLENGES AND SOLUTIONS

### Challenge 1: 64-bit Arithmetic Overflow

**Problem**: `(reference_area_size * x) >> 32` could overflow U64.

**Solution**:
- Use U64_Mod for intermediate calculations
- Add explicit bound assertions
- Leverage SMT solver to prove `reference_area_size <= 2^17` → product fits

**Proof obligation**:
```ada
pragma Assert (U64(Reference_Area_Size) * X <= U64'Last);
-- Proof: Reference_Area_Size <= 131072 = 2^17
--        X <= 2^32
--        Product <= 2^49 < 2^64 ✓
```

### Challenge 2: Modulo Wraparound

**Problem**: `(start_position + relative_position) mod lane_length` requires proof of correctness.

**Solution**:
- Use bounded subtypes: `start_position, relative_position < lane_length`
- SPARK automatically proves `(a + b) mod n` when `a, b < n`

### Challenge 3: Reference Area Size = 0

**Problem**: If `Index = 0` and `Pass = 0` and `Segment = 0`, then `reference_area_size = Index - 1 = -1` (underflow).

**Solution**:
```ada
-- First two blocks are special-cased (filled by Init)
Starting_Index := (if Pos.Pass = 0 and Pos.Segment = 0 then 2 else 0);

-- Thus Index >= 2 in first segment, so reference_area_size >= 1 ✓
```

### Challenge 4: Address Block Regeneration

**Problem**: Must regenerate addresses every 128 blocks (stateful).

**Solution**:
```ada
-- Use loop iteration counter
if Data_Independent_Addressing then
   if Index mod 128 = 0 then
      Generate_Next_Addresses(Address_State);
   end if;
   Pseudo_Rand := Address_State.Address_Block(Index mod 128);
end if;

pragma Loop_Invariant (Address_State.Counter = Index mod 128);
```

---

## 8. ESTIMATED VC COUNT

| Component | VCs | Complexity |
|-----------|-----|------------|
| Extract_J1_J2 | 2 | Trivial (bit operations) |
| Calculate_Ref_Lane | 3 | Simple (modulo + conditional) |
| Calculate_Reference_Area_Size | 12 | Moderate (many conditionals) |
| Map_J1_To_Position | 8 | HIGH (64-bit arithmetic proofs) |
| Calculate_Ref_Index | 6 | Moderate (modulo wraparound) |
| Initialize_Address_Generator | 4 | Simple (assignments) |
| Get_Next_Pseudo_Rand | 5 | Moderate (stateful) |
| **Total** | **40** | **Target achieved** |

**Breakdown by category**:
- Range checks: 15 VCs
- Overflow checks: 10 VCs
- Postcondition proofs: 10 VCs
- Loop invariants: 5 VCs

**Expected proof rate**: 100% (40/40)

**Rationale**: All arithmetic is bounded, all indices use subtypes, all complex operations have explicit contracts.

---

## 9. IMPLEMENTATION ROADMAP

### Phase 2.6.1: Core Functions (Week 1)

1. Implement `Extract_J1_J2`
2. Implement `Calculate_Ref_Lane`
3. Implement `Calculate_Reference_Area_Size`
4. Verify with GNATprove (target: 20/20 VCs)

### Phase 2.6.2: Mapping Function (Week 1)

1. Implement `Map_J1_To_Position` with U64_Mod
2. Add explicit overflow assertions
3. Prove arithmetic bounds with SMT solver
4. Verify with GNATprove (target: 8/8 VCs)

### Phase 2.6.3: Address Generation (Week 1)

1. Implement `Initialize_Address_Generator`
2. Implement `Get_Next_Pseudo_Rand`
3. Add loop invariants for stateful regeneration
4. Verify with GNATprove (target: 9/9 VCs)

### Phase 2.6.4: Integration (Week 1)

1. Create main `Calculate_Ref_Index` function
2. Add comprehensive postconditions
3. Write unit tests with known vectors
4. Full verification (target: 40/40 VCs)

**Total timeline**: 1 week for Phase 2.6 (indexing functions only)

---

## 10. TEST VECTORS

### 10.1 RFC 9106 Appendix A

Use the official test vectors to validate index calculations:

```
Memory: 32 KiB (32 blocks)
Parallelism: 4
Iterations: 3
Passes: 3

Expected indices for block [0][2]:
  Pass 0, Segment 0, Index 2:
    J1 = <extract from address_block[2]>
    J2 = <extract from address_block[2]>
    ref_lane = J2 mod 4 = 0 (must be same lane)
    ref_index = index_alpha(...) = 0 or 1 (only options)
```

### 10.2 Property-Based Tests

```ada
-- Property: ref_index always < lane_length
for all Pos, Index =>
   ref_index := Calculate_Ref_Index(...);
   assert ref_index < Active_Blocks_Per_Lane;

-- Property: ref_index never equals current block
for all Pos, Index =>
   ref_index := Calculate_Ref_Index(...);
   current := Pos.Segment * Active_Blocks_Per_Segment + Index;
   assert ref_index /= current;

-- Property: First segment only references earlier blocks
for all Index when Pos.Pass = 0 and Pos.Segment = 0 =>
   ref_index := Calculate_Ref_Index(...);
   assert ref_index < Index;
```

---

## 11. RESOURCES

### Specifications

- **RFC 9106 (Argon2)**: https://www.rfc-editor.org/rfc/rfc9106.html
  - Section 3.3: Memory organization and segments
  - Section 3.4: Indexing functions (Argon2i, Argon2d, Argon2id)
  - Section 3.4.2: The mapping function (index_alpha)

### Reference Implementation

- **phc-winner-argon2**: https://github.com/P-H-C/phc-winner-argon2
  - `src/ref.c`: fill_segment, next_addresses
  - `src/core.c`: index_alpha

### Academic Papers

- **Argon2 Specification** (original): https://password-hashing.net/argon2-specs.pdf
  - Section 3.4: Complete mathematical description of indexing

---

## 12. SUCCESS CRITERIA

### Functional Correctness

- [ ] All 40 VCs proven (100% proof rate)
- [ ] Matches C reference implementation behavior
- [ ] Passes RFC 9106 test vectors
- [ ] Properties verified: ref_index always valid, never self-reference

### Code Quality

- [ ] All functions < 50 SLOC (maintainability)
- [ ] No magic numbers (all constants named)
- [ ] Comprehensive comments with RFC citations
- [ ] Pre/Post contracts on every function

### Performance

- [ ] Index calculation < 100 CPU cycles (negligible overhead)
- [ ] No heap allocation (stack only)
- [ ] Inlinable helper functions

---

## NEXT STEPS

1. **Implement `sparkpass-crypto-argon2id-index.ads`** (see next document)
2. **Implement `sparkpass-crypto-argon2id-index.adb`**
3. **Run GNATprove** with `--level=2` and `--timeout=30`
4. **Iterate on failing VCs** with explicit assertions
5. **Validate with test vectors** from RFC 9106
6. **Proceed to Phase 2.7** (Fill Memory)

---

**END OF RESEARCH DOCUMENT**

**Estimated Reading Time**: 30 minutes
**Estimated Implementation Time**: 1 week
**Confidence Level**: HIGH (algorithm is well-specified and deterministic)
