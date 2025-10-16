pragma SPARK_Mode (Off);
with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

package Bindings.POSIX is
   pragma Preelaborate;

   --  POSIX file operations for atomic writes

   --  fsync: synchronize file's in-core state with storage device
   function fsync (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "fsync";

   --  open: open file descriptor (for fsync on directory)
   O_RDONLY : constant := 0;

   function open
     (pathname : Interfaces.C.Strings.chars_ptr;
      flags    : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "open";

   --  close: close file descriptor
   function close (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "close";

   --  rename: atomically rename file
   function rename
     (oldpath : Interfaces.C.Strings.chars_ptr;
      newpath : Interfaces.C.Strings.chars_ptr) return Interfaces.C.int
     with Import, Convention => C, External_Name => "rename";

   --  fileno: get file descriptor from FILE*
   type FILE is null record;
   pragma Convention (C, FILE);
   type FILE_Access is access all FILE;
   pragma Convention (C, FILE_Access);

   function fileno (stream : FILE_Access) return Interfaces.C.int
     with Import, Convention => C, External_Name => "fileno";

   --  isatty: test if file descriptor refers to a terminal
   function isatty (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "isatty";

   --  getuid: get real user ID
   function getuid return Interfaces.C.unsigned
     with Import, Convention => C, External_Name => "getuid";

   --  stat: get file status
   --  macOS struct stat field layout (from sys/stat.h)
   --  Fields must match exact C layout for correct memory alignment
   --  Total size: 144 bytes on macOS
   type stat_t is record
      st_dev    : Interfaces.Unsigned_32;          -- offset 0, dev_t (4 bytes)
      st_mode   : Interfaces.Unsigned_16;          -- offset 4, mode_t (2 bytes)
      st_nlink  : Interfaces.Unsigned_16;          -- offset 6, nlink_t (2 bytes)
      st_ino    : Interfaces.Unsigned_64;          -- offset 8, ino_t (8 bytes)
      st_uid    : Interfaces.C.unsigned;           -- offset 16, uid_t (4 bytes)
      st_gid    : Interfaces.C.unsigned;           -- offset 20, gid_t (4 bytes)
      --  Remaining fields omitted but structure size must be 144 bytes total
      st_padding : Interfaces.C.char_array (1 .. 120);  -- 144 - 24 = 120 bytes padding
   end record
     with Convention => C;

   function stat
     (pathname : Interfaces.C.Strings.chars_ptr;
      buf      : access stat_t) return Interfaces.C.int
     with Import, Convention => C, External_Name => "stat";

   --  lstat: get file status (does not follow symlinks)
   function lstat
     (pathname : Interfaces.C.Strings.chars_ptr;
      buf      : access stat_t) return Interfaces.C.int
     with Import, Convention => C, External_Name => "lstat";

   --  chmod: change file permissions
   function chmod
     (pathname : Interfaces.C.Strings.chars_ptr;
      mode     : Interfaces.C.unsigned) return Interfaces.C.int
     with Import, Convention => C, External_Name => "chmod";

   --  File permission bits
   S_IRWXU : constant := 8#0700#;  -- Owner: read, write, execute
   S_IRUSR : constant := 8#0400#;  -- Owner: read
   S_IWUSR : constant := 8#0200#;  -- Owner: write
   S_IRWXG : constant := 8#0070#;  -- Group: read, write, execute
   S_IRWXO : constant := 8#0007#;  -- Other: read, write, execute
   S_IFMT  : constant := 8#0170000#;  -- File type mask
   S_IFREG : constant := 8#0100000#;  -- Regular file

end Bindings.POSIX;
