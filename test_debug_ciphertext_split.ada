-- Quick calculation check
-- ML-KEM-1024 ciphertext structure:
-- c = c₁ || c₂
-- c₁ = Encode₁₁(Compress₁₁(u))  -- u is 4×256 coefficients, each 11 bits
--    = 4 × 256 × 11 / 8 = 4 × 352 = 1408 bytes
-- c₂ = Encode₅(Compress₅(v))     -- v is 256 coefficients, each 5 bits
--    = 256 × 5 / 8 = 160 bytes
-- Total = 1408 + 160 = 1568 bytes

-- In Encaps.adb line 220-221:
--   Ciphertext(1 .. 1408) := C1_Bytes;
--   Ciphertext(1409 .. 1568) := C2_Bytes;

-- In Decaps.adb line 56-58:
--   C1_Bytes := Ciphertext(1 .. 1408);
--   C2_Bytes := Ciphertext(1409 .. 1568);

-- This looks correct!

-- But check the constants:
-- Bytes_Per_Vector_11 = 4 * Bytes_Per_Poly_11 = 4 * 352 = 1408 ✓
-- Bytes_Per_Poly_5 = 160 ✓
