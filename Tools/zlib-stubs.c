
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <string.h>
#include <zlib.h>

struct buffered_stream
{
  z_stream z;
  void     *in_buf;
  void     *out_buf;
  size_t   in_size;
  size_t   out_size;
};

#define Stream_val(v) ((struct buffered_stream *)Data_custom_val(v))

void finalize_stream(value zstream)
{
  struct buffered_stream *zs;

  zs = Stream_val(zstream);

  free(zs->in_buf);
  free(zs->out_buf);
};

static struct custom_operations zstream_ops =
{
  "zstream",
  finalize_stream,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value alloc_zstream(int buffer_size)
{
  CAMLparam0();
  CAMLlocal1(zstream);
  struct buffered_stream *zs;

  zstream = alloc_custom(&zstream_ops, sizeof(struct buffered_stream), 0, 1);

  zs = Stream_val(zstream);

  zs->in_buf      = NULL;
  zs->out_buf     = malloc(buffer_size);
  zs->out_size    = buffer_size;
  zs->in_size     = 0;
  zs->z.next_in   = NULL;
  zs->z.avail_in  = 0;
  zs->z.next_out  = zs->out_buf;
  zs->z.avail_out = buffer_size;
  zs->z.zalloc    = Z_NULL;
  zs->z.zfree     = Z_NULL;
  zs->z.opaque    = Z_NULL;

  CAMLreturn(zstream);
}

CAMLprim value zlib_set_input(value stream, value string)
{
  CAMLparam2(stream, string);
  CAMLlocal1(buf);
  struct buffered_stream *zs;

  zs = Stream_val(stream);

  free(zs->in_buf);

  zs->in_buf = zs->z.next_in = malloc(string_length(string));

  if (!zs->in_buf)
  {
    zs->z.avail_in = 0;
    failwith("not enough memory");
  }

  zs->z.avail_in = string_length(string);

  memcpy(zs->in_buf, String_val(string), zs->z.avail_in);

  CAMLreturn(Val_unit);
};

CAMLprim value zlib_get_output(value stream)
{
  CAMLparam1(stream);
  CAMLlocal1(block);
  struct buffered_stream *zs;

  zs    = Stream_val(stream);
  block = alloc_string(zs->out_size - zs->z.avail_out);
  zs    = Stream_val(stream);

  memcpy(String_val(block), zs->out_buf, zs->out_size - zs->z.avail_out);

  zs->z.next_out  = zs->out_buf;
  zs->z.avail_out = zs->out_size;

  CAMLreturn(block);
};

CAMLprim value zlib_avail_input(value stream)
{
  CAMLparam1(stream);

  CAMLreturn(Val_int(Stream_val(stream)->z.avail_in));
};

CAMLprim value zlib_deflate_init(value buffer_size, value level)
{
  CAMLparam2(buffer_size, level);
  CAMLlocal1(block);
  struct buffered_stream *zs;

  block = alloc_zstream(Int_val(buffer_size));

  zs = Stream_val(block);

  if (deflateInit(&zs->z, Int_val(level)) != Z_OK)
    failwith (zs->z.msg);

  CAMLreturn(block);
}

CAMLprim value zlib_deflate(value stream, value flush)
{
  CAMLparam2(stream, flush);

  deflate(&Stream_val(stream)->z, Int_val(flush));

  CAMLreturn(Val_unit);
}

CAMLprim value zlib_deflate_end(value stream)
{
  CAMLparam1(stream);

  deflateEnd(&Stream_val(stream)->z);

  CAMLreturn(Val_unit);
}

CAMLprim value zlib_inflate_init(value buffer_size)
{
  CAMLparam1(buffer_size);
  CAMLlocal1(block);
  struct buffered_stream *zs;

  block = alloc_zstream(Int_val(buffer_size));

  zs = Stream_val(block);

  if (inflateInit(&zs->z) != Z_OK)
    failwith (zs->z.msg);

  CAMLreturn(block);
}

CAMLprim value zlib_inflate(value stream, value flush)
{
  CAMLparam2(stream, flush);

  inflate(&Stream_val(stream)->z, Int_val(flush));

  CAMLreturn(Val_unit);
}

CAMLprim value zlib_inflate_end(value stream)
{
  CAMLparam1(stream);

  inflateEnd(&Stream_val(stream)->z);

  CAMLreturn(Val_unit);
}

CAMLprim value zlib_compress(value dest, value source)
{
  CAMLparam2(dest, source);
  uLongf len = string_length(dest);

  if (compress(String_val(dest), &len, String_val(source), string_length(source)) != Z_OK)
    failwith("compress");

  CAMLreturn(Val_int(len));
};

CAMLprim value zlib_compress2(value dest, value source, value level)
{
  CAMLparam3(dest, source, level);
  uLongf len = string_length(dest);

  if (compress2(String_val(dest), &len, String_val(source), string_length(source), Int_val(level)) != Z_OK)
    failwith("compress2");

  CAMLreturn(Val_int(len));
};

CAMLprim value zlib_uncompress(value dest, value source)
{
  CAMLparam2(dest, source);
  uLongf len = string_length(dest);

  if (uncompress(String_val(dest), &len, String_val(source), string_length(source)) != Z_OK)
    failwith("uncompress");

  CAMLreturn(Val_int(len));
};

CAMLprim value zlib_adler32(value adler, value buf)
{
  CAMLparam2(adler, buf);

  CAMLreturn(copy_int32(adler32(Int32_val(adler), String_val(buf), string_length(buf))));
};

CAMLprim value zlib_crc32(value crc, value buf)
{
  CAMLparam2(crc, buf);

  CAMLreturn(copy_int32(crc32(Int32_val(crc), String_val(buf), string_length(buf))));
};

