/*
**      ==================================
**      OOOO   OOOO OOOO  O      O   OOOO
**      O   O  O    O     O     O O  O   O
**      O   O  O    O     O     O O  O   O
**      OOOO   OOOO OOOO  O     OOO  OOOO
**      O   O     O    O  O    O   O O   O
**      O   O     O    O  O    O   O O   O
**      OOOO   OOOO OOOO  OOOO O   O OOOO
**      ================================== 
**      BSSLAB, Dr. Stefan Bosse www.bsslab.de
**
**    PROTECTED BY AND DISTRIBUTED UNDER THE TERMS OF: 
**    Free Software Foundation-Europe, GNU GPL License, Version 2
**
**    $MODIFIEDBY:  BSSLAB
**    $AUTHORS:     Xavier Leroy, Stefan Bosse
**    $INITIAL:     (C) 1996 INRIA
**    $CREATED:     
**    $MODIFIED:    
**    $VERSION:     1.01
**
**    $INFO:
**
**
**
**    $ENDOFINFO
**
*/




#include <mlvalues.h>
#include <memory.h>
#include <alloc.h>
#include "unixsupport.h"
#include "cst2constr.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fail.h>

#ifdef HAS_STAT

#ifndef S_IFLNK
#define S_IFLNK 0
#endif
#ifndef S_IFIFO
#define S_IFIFO 0
#endif
#ifndef S_IFSOCK
#define S_IFSOCK 0
#endif
#ifndef S_IFBLK
#define S_IFBLK 0
#endif

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

static value stat_aux(struct stat *buf)
{
  value v;
  value atime = Val_unit, mtime = Val_unit, ctime = Val_unit;

  Begin_roots3(atime,mtime,ctime)
    atime = copy_double((double) buf->st_atime);
    mtime = copy_double((double) buf->st_mtime);
    ctime = copy_double((double) buf->st_ctime);
    v = alloc_small(12, 0);
    Field (v, 0) = Val_int (buf->st_dev);
    Field (v, 1) = Val_int (buf->st_ino);
    Field (v, 2) = cst_to_constr(buf->st_mode & S_IFMT, file_kind_table,
                                 sizeof(file_kind_table) / sizeof(int), 0);
    Field (v, 3) = Val_int(buf->st_mode & 07777);
    Field (v, 4) = Val_int (buf->st_nlink);
    Field (v, 5) = Val_int (buf->st_uid);
    Field (v, 6) = Val_int (buf->st_gid);
    Field (v, 7) = Val_int (buf->st_rdev);
    Field (v, 8) = Val_int (buf->st_size);
    Field (v, 9) = atime;
    Field (v, 10) = mtime;
    Field (v, 11) = ctime;
  End_roots();
  return v;
}

CAMLprim value unix_stat(value path)
{
  int ret;
  struct stat buf;
  ret = stat(String_val(path), &buf);
  if (ret == -1) uerror("stat", path);
  return stat_aux(&buf);
}

CAMLprim value unix_lstat(value path)
{
  int ret;
  struct stat buf;
#ifdef HAS_SYMLINK
  ret = lstat(String_val(path), &buf);
#else
  ret = stat(String_val(path), &buf);
#endif
  if (ret == -1) uerror("lstat", path);
  return stat_aux(&buf);
}

CAMLprim value unix_fstat(value fd)
{
  int ret;
  struct stat buf;
  ret = fstat(Int_val(fd), &buf);
  if (ret == -1) uerror("fstat", Nothing);
  return stat_aux(&buf);
}

#else /* !HAS_STAT */

CAMLprim value unix_stat(value path)
{
    invalid_argument("stat not implemented");  
    return Val_unit;
}

CAMLprim value unix_lstat(value path)
{
    invalid_argument("lstat not implemented");  
    return Val_unit;
}

CAMLprim value unix_fstat(value fd)
{
    invalid_argument("fstat not implemented");  
}

#endif  /* !HAS_STAT */