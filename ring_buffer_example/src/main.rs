mod ring_buffer;
mod ring_buffer_seq;
use ring_buffer::RingBuf;
use ring_buffer_seq::RingBufSeq;
use std::mem::size_of;
use std::thread;
use std::time::Instant;

trait RingBufferApi: Send + Sync + Clone + 'static {
    fn new(count: usize) -> Self;
    fn get_space_buf(&self) -> (u64, usize);
    fn produce(&self, count: usize) -> bool;
    fn get_data_buf(&self) -> (u64, usize);
    fn consume(&self, count: usize) -> bool;
    fn write(&mut self, buf: &[u8]) -> Result<(bool, usize), ring_buffer::Error>;
    fn read(&self, buf: &mut [u8]) -> Result<(bool, usize), ring_buffer::Error>;
}

impl RingBufferApi for RingBuf {
    fn new(count: usize) -> Self {
        RingBuf::new(count)
    }
    fn get_space_buf(&self) -> (u64, usize) {
        self.get_space_buf()
    }
    fn produce(&self, count: usize) -> bool {
        self.produce(count)
    }
    fn get_data_buf(&self) -> (u64, usize) {
        self.get_data_buf()
    }
    fn consume(&self, count: usize) -> bool {
        self.consume(count)
    }
    fn write(&mut self, buf: &[u8]) -> Result<(bool, usize), ring_buffer::Error> {
        self.write(buf)
    }
    fn read(&self, buf: &mut [u8]) -> Result<(bool, usize), ring_buffer::Error> {
        self.read(buf)
    }
}

impl RingBufferApi for RingBufSeq {
    fn new(count: usize) -> Self {
        RingBufSeq::new(count)
    }
    fn get_space_buf(&self) -> (u64, usize) {
        self.get_space_buf()
    }
    fn produce(&self, count: usize) -> bool {
        self.produce(count)
    }
    fn get_data_buf(&self) -> (u64, usize) {
        self.get_data_buf()
    }
    fn consume(&self, count: usize) -> bool {
        self.consume(count)
    }
    fn write(&mut self, buf: &[u8]) -> Result<(bool, usize), ring_buffer::Error> {
        self.write(buf)
            .map_err(|_| ring_buffer::Error::SysError(ring_buffer::SysErr::EINVAL))
    }
    fn read(&self, buf: &mut [u8]) -> Result<(bool, usize), ring_buffer::Error> {
        self.read(buf)
            .map_err(|_| ring_buffer::Error::SysError(ring_buffer::SysErr::EINVAL))
    }
}

fn print_header(title: &str) {
    println!("\n================================================================================");
    println!("{}", title);
    println!("--------------------------------------------------------------------------------");
}

fn run_benchmark_raw<T: RingBufferApi>(name: &str, batch_size: usize) {
    let count = 131072; // 1MB buffer
    let ring = T::new(count);
    let total_bytes = 1024 * 1024 * 128; // 1GB

    let producer_ring = ring.clone();
    let consumer_ring = ring.clone();

    // println!(
    //     "Starting raw benchmark (get_space_buf/get_data_buf) for {}: 1GB transfer, 1MB ring buffer",
    //     name
    // );
    let start = Instant::now();

    let producer_affinity = core_affinity::get_core_ids().unwrap()[12];
    let consumer_affinity = core_affinity::get_core_ids().unwrap()[14];

    let producer = thread::spawn(move || {
        core_affinity::set_for_current(producer_affinity);
        let mut produced = 0;
        while produced < total_bytes {
            let (addr, len) = producer_ring.get_space_buf();
            if len > 0 {
                // len is in bytes.
                // We want to transfer u64 elements.
                let len_items = len / 8;
                let total_items = total_bytes / 8;
                let produced_items = produced / 8;

                let to_write_items = std::cmp::min(len_items, total_items - produced_items);

                if to_write_items > 0 {
                    let ptr = addr as *mut u64;
                    unsafe {
                        for i in 0..to_write_items {
                            *ptr.add(i) = (produced_items + i) as u64;
                            if (i + 1) % batch_size == 0 {
                                producer_ring.produce(batch_size * size_of::<u64>());
                            }
                        }
                        producer_ring.produce(to_write_items % batch_size * size_of::<u64>());
                    }
                    produced += to_write_items * 8;
                } else {
                    thread::yield_now();
                }
            } else {
                thread::yield_now();
            }
        }
    });

    let consumer = thread::spawn(move || {
        core_affinity::set_for_current(consumer_affinity);
        let mut consumed = 0;
        while consumed < total_bytes {
            let (addr, len) = consumer_ring.get_data_buf();
            if len > 0 {
                let consumed_items = consumed / 8;
                let len_items = len / 8;

                if len_items > 0 {
                    let ptr = addr as *const u64;
                    unsafe {
                        // Verify data
                        for i in 0..len_items {
                            let expected = (consumed_items + i) as u64;
                            let val = *ptr.add(i);
                            if val != expected {
                                panic!(
                                    "Mismatch at index {}: expected {}, got {}",
                                    consumed_items + i,
                                    expected,
                                    val
                                );
                            }
                            if (i + 1) % batch_size == 0 {
                                consumer_ring.consume(batch_size * size_of::<u64>());
                            }
                        }
                        consumer_ring.consume(len_items % batch_size * size_of::<u64>());
                    }
                    consumed += len_items * 8;
                } else {
                    thread::yield_now();
                }
            } else {
                thread::yield_now();
            }
        }
    });

    producer.join().unwrap();
    consumer.join().unwrap();

    let duration = start.elapsed();
    let mb = total_bytes as f64 / 1024.0 / 1024.0;
    let seconds = duration.as_secs_f64();
    // println!("{}: Transferred {} MB in {:.4} seconds", name, mb, seconds);
    println!("{:<35} : {:>8.2} MB/s", name, mb / seconds);
    // println!("--------------------------------------------------");
}

fn run_benchmark_read_write<T: RingBufferApi>(name: &str) {
    let count = 131072; // 1MB buffer
    let ring = T::new(count);
    let total_bytes = 1024 * 1024 * 8; // 8MB
    let chunk_size = 1; // 4KB

    let mut producer_ring = ring.clone();
    let consumer_ring = ring.clone();

    // println!(
    //     "Starting read/write benchmark for {}: 1GB transfer, 1MB ring buffer, {}B chunks",
    //     name, chunk_size
    // );
    let start = Instant::now();

    let producer_affinity = core_affinity::get_core_ids().unwrap()[12];
    let consumer_affinity = core_affinity::get_core_ids().unwrap()[14];

    let producer = thread::spawn(move || {
        core_affinity::set_for_current(producer_affinity);
        let data = vec![1u8; chunk_size];
        let mut written = 0;
        while written < total_bytes {
            let remaining = total_bytes - written;
            let to_write = if remaining < chunk_size {
                remaining
            } else {
                chunk_size
            };

            match producer_ring.write(&data[0..to_write]) {
                Ok((_, n)) => {
                    written += n;
                    if n == 0 {
                        thread::yield_now();
                    }
                }
                Err(_) => {
                    thread::yield_now();
                }
            }
        }
    });

    let consumer = thread::spawn(move || {
        core_affinity::set_for_current(consumer_affinity);
        let mut data = vec![0u8; chunk_size];
        let mut read = 0;
        while read < total_bytes {
            match consumer_ring.read(&mut data) {
                Ok((_, n)) => {
                    read += n;
                    if n == 0 {
                        thread::yield_now();
                    }
                }
                Err(_) => {
                    thread::yield_now();
                }
            }
        }
    });

    producer.join().unwrap();
    consumer.join().unwrap();

    let duration = start.elapsed();
    let mb = total_bytes as f64 / 1024.0 / 1024.0;
    let seconds = duration.as_secs_f64();
    // println!("{}: Transferred {} MB in {:.4} seconds", name, mb, seconds);
    println!("{:<35} : {:>8.2} MB/s", name, mb / seconds);
    // println!("--------------------------------------------------");
}

fn main() {
    print_header("Raw Benchmark (Batch Size: 1)");
    run_benchmark_raw::<RingBuf>("RingBuf (Optimized)", 1);
    run_benchmark_raw::<RingBufSeq>("RingBufSeq (Sequential)", 1);

    print_header("Raw Benchmark (Batch Size: MAX)");
    run_benchmark_raw::<RingBuf>("RingBuf (Optimized)", usize::MAX);
    run_benchmark_raw::<RingBufSeq>("RingBufSeq (Sequential)", usize::MAX);

    print_header("Read/Write Benchmark (1B chunks)");
    run_benchmark_read_write::<RingBuf>("RingBuf (Optimized)");
    run_benchmark_read_write::<RingBufSeq>("RingBufSeq (Sequential)");

    print_header("Deadlock Test (Producer/Consumer sleep/wake)");
    println!(
        "Expectation: RingBuf (Acquire/Release) may deadlock. RingBufSeq (SeqCst) should not."
    );

    // Uncomment to run deadlock test. It might hang forever!
    // run_deadlock_test::<RingBuf>("RingBuf (Optimized)");
    print!("\nRunning RingBufSeq (Sequential)... ");
    use std::io::Write;
    std::io::stdout().flush().unwrap();
    run_deadlock_test::<RingBufSeq>("RingBufSeq (Sequential)");
    println!("PASSED");

    print!("Running RingBuf (Optimized)...     ");
    std::io::stdout().flush().unwrap();
    run_deadlock_test::<RingBuf>("RingBuf (Optimized)");
    println!("PASSED (Unexpected)");
}

fn run_deadlock_test<T: RingBufferApi>(_name: &str) {
    let count = 131072; // 1MB buffer
    let ring = T::new(count);
    let total_bytes = 1024 * 1024 * 100; // 100MB
    let batch_size = 1;

    let producer_ring = ring.clone();
    let consumer_ring = ring.clone();

    // println!("Starting deadlock test for {}: 100MB transfer", name);
    let _start = Instant::now();
    // ... rest of implementation remains the same until println ...
    let producer_affinity = core_affinity::get_core_ids().unwrap()[12];
    let consumer_affinity = core_affinity::get_core_ids().unwrap()[14];

    use std::sync::{Arc, Barrier, Condvar, Mutex};
    // producer_cv: Producer waits on this when buffer is full. Consumer notifies this when it consumes.
    let producer_cv = Arc::new((Mutex::new(()), Condvar::new()));
    // consumer_cv: Consumer waits on this when buffer is empty. Producer notifies this when it produces.
    let consumer_cv = Arc::new((Mutex::new(()), Condvar::new()));
    let barrier = Arc::new(Barrier::new(2));

    let p_cv = producer_cv.clone();
    let c_cv = consumer_cv.clone();
    let b_prod = barrier.clone();

    let producer = thread::spawn(move || {
        core_affinity::set_for_current(producer_affinity);
        b_prod.wait();

        let mut produced = 0;
        let mut guard = p_cv.0.lock().unwrap();
        while produced < total_bytes {
            let (addr, len) = producer_ring.get_space_buf();
            if len == 0 {
                guard = p_cv.1.wait(guard).unwrap();
                continue;
            }

            // Release lock while processing to allow consumer to notify
            drop(guard);

            // len is in bytes.
            let len_items = len / 8;
            let total_items = total_bytes / 8;
            let produced_items = produced / 8;

            let to_write_items = std::cmp::min(len_items, total_items - produced_items);

            if to_write_items > 0 {
                let ptr = addr as *mut u64;
                unsafe {
                    for i in 0..to_write_items {
                        *ptr.add(i) = (produced_items + i) as u64;
                        if (i + 1) % batch_size == 0 {
                            if producer_ring.produce(batch_size * size_of::<u64>()) {
                                let (lock, cvar) = &*c_cv;
                                let _g = lock.lock().unwrap();
                                cvar.notify_one();
                            }
                        }
                    }
                    let rem = to_write_items % batch_size;
                    if rem > 0 {
                        if producer_ring.produce(rem * size_of::<u64>()) {
                            let (lock, cvar) = &*c_cv;
                            let _g = lock.lock().unwrap();
                            cvar.notify_one();
                        }
                    }
                }
                produced += to_write_items * 8;
            }
            // Re-acquire lock for next iteration check
            guard = p_cv.0.lock().unwrap();
        }
    });

    let p_cv_c = producer_cv.clone();
    let c_cv_c = consumer_cv.clone();
    let b_cons = barrier.clone();

    let consumer = thread::spawn(move || {
        core_affinity::set_for_current(consumer_affinity);
        b_cons.wait();

        let mut consumed = 0;
        let mut guard = c_cv_c.0.lock().unwrap();
        while consumed < total_bytes {
            let (addr, len) = consumer_ring.get_data_buf();
            if len == 0 {
                guard = c_cv_c.1.wait(guard).unwrap();
                continue;
            }

            // Release lock while processing
            drop(guard);

            let consumed_items = consumed / 8;
            let len_items = len / 8;

            if len_items > 0 {
                let ptr = addr as *const u64;
                unsafe {
                    for i in 0..len_items {
                        let expected = (consumed_items + i) as u64;
                        let val = *ptr.add(i);
                        if val != expected {
                            panic!("Mismatch");
                        }
                        if (i + 1) % batch_size == 0 {
                            if consumer_ring.consume(batch_size * size_of::<u64>()) {
                                let (lock, cvar) = &*p_cv_c;
                                let _g = lock.lock().unwrap();
                                cvar.notify_one();
                            }
                        }
                    }
                    let rem = len_items % batch_size;
                    if rem > 0 {
                        if consumer_ring.consume(rem * size_of::<u64>()) {
                            let (lock, cvar) = &*p_cv_c;
                            let _g = lock.lock().unwrap();
                            cvar.notify_one();
                        }
                    }
                }
                consumed += len_items * 8;
            }
            // Re-acquire lock for next iteration check
            guard = c_cv_c.0.lock().unwrap();
        }
    });

    producer.join().unwrap();
    consumer.join().unwrap();

    // let duration = start.elapsed();
    // let mb = total_bytes as f64 / 1024.0 / 1024.0;
    // let seconds = duration.as_secs_f64();
    // println!("{}: Transferred {} MB in {:.4} seconds", name, mb, seconds);
}
