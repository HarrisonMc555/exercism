use std::io::{Read, Result, Write};

pub struct ReadStats<R>{
    reader: R,
    num_bytes: usize,
    num_reads: usize,
    _marker: ::std::marker::PhantomData<R>,
}

impl<R: Read> ReadStats<R> {
    pub fn new(reader: R) -> ReadStats<R> {
        ReadStats {
            reader,
            num_bytes: 0,
            num_reads: 0,
            _marker: ::std::marker::PhantomData,
        }
    }

    pub fn get_ref(&self) -> &R {
        &self.reader
    }

    pub fn bytes_through(&self) -> usize {
        self.num_bytes
    }

    pub fn reads(&self) -> usize {
        self.num_reads
    }
}

impl<R: Read> Read for ReadStats<R> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let result = self.reader.read(buf);
        if let Ok(nbytes) = result {
            self.num_bytes += nbytes;
        }
        self.num_reads += 1;
        result
    }
}

pub struct WriteStats<W>{
    writer: W,
    num_bytes: usize,
    num_writes: usize,
    _marker: ::std::marker::PhantomData<W>,
}

impl<W: Write> WriteStats<W> {
    pub fn new(writer: W) -> WriteStats<W> {
        WriteStats {
            writer,
            num_bytes: 0,
            num_writes: 0,
            _marker: ::std::marker::PhantomData,
        }
    }

    pub fn get_ref(&self) -> &W {
        &self.writer
    }

    pub fn bytes_through(&self) -> usize {
        self.num_bytes
    }

    pub fn writes(&self) -> usize {
        self.num_writes
    }
}

impl<W: Write> Write for WriteStats<W> {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let result = self.writer.write(buf);
        if let Ok(nbytes) = result {
            self.num_bytes += nbytes;
        }
        self.num_writes += 1;
        result
    }

    fn flush(&mut self) -> Result<()> {
        self.writer.flush()
    }
}
