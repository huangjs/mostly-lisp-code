;;; SSTable is a simple abstraction to efficiently store large number
;;; of key-value pairs while optimizing for high throughput,
;;; sequential read/write workloads.

;;; A simple but also a useful way to exchange large, sorted data
;;; segments.

;;; However, random writes are harder and expensive, unless the entire
;;; table is in memory. Log Structured Merge Trees are used as the
;;; savior.

;;; MemTable <--1*n--> SS Indices <--1*1--> SSTable (immutable)

;;; 1. On-disk SSTable indexes are always loaded into memory
;;; 2. All writes go directly to the MemTable index
;;; 3. Reads check the MemTable first and then the SSTable indexes
;;; 4. Periodically, the MemTable is flushed to disk as an SSTable
;;; 5. Periodically, on-disk SSTables are "collapsed together"

;;; Once the SSTable is on disk, it is immutable, hence updates and
;;; deletes can't touch the data. Instead, a more recent value is
;;; simply stored in MemTable in case of update, and a "tombstone"
;;; record is appended for deletes.

;;; Finally, having hundreds of on-disk SSTables is also not a great
;;; idea, hence periodically we will run a process to merge the
;;; on-disk SSTables, at which time the update and delete records will
;;; overwrite and remove the older data.

