#RabbitMQ Prometheus.io exporter

Implemented as RabbitMQ Management Plugin plugin.
Also exports Erlang VM amd process metrics (~ 80 metrics in total).

Implemented using [Erlang Prometheus.io client](https://github.com/deadtrickster/prometheus.erl)

![rabbitmq prometheus exporter grafana dashboard](http://i.imgur.com/tWiDw56.png?1)

## TOC
 - [Versioning](#versioning)
 - [Installation](#installation)
 - [Configuration](#configuration)
 - [Metrics](#metrics)
   - [RabbitMQ specific metrics](#rabbitmq-specific-metrics)
     - [Overview](#overview)
     - [Queues](#queues)
     - [Exchanges](#exchanges)
   - [Erlang VM metrics](#erlang-vm-metrics)
     - [System Info](#system-info)
     - [Statistics](#statistics)
     - [Memory](#memory)
   - [Process metrics](#process-metrics)
 - [License](#license)

## Versioning

While RabbitMQ transitions from webmachine to cowboy we maintain two branches one for 3.6.x and one for 3.7.x.
Plugin version should be read as follows: 3.7.1.x - where 3.7.1 is required RabbitMQ version and x is just incremental version of the plugin.

## Installation

 - [Release for RabbitMQ 3.6.5](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.6.5.2)
 - [Release for RabbitMQ 3.7](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.7_1)

Download suitable version and follow regular [RabbitMQ plugin installation instructions](http://www.rabbitmq.com/installing-plugins.html).

Do not forget to fire `rabbitmq-plugins enable`!

## Configuration

This exporter supports the following options via `rabbitmq_exporter` entry of `prometheus` app env:
 - `path` - scrape endpoint. Default is `"metrics"`. Note RabbitMQ translates this to `"/api/metrics"`;
 - `format` - scrape format. Default is `prometheus_text_format`;
 - `exchange_messages_stat` - same as `queue_messages_state` but for the exchanges;
 - `queue_messages_stat` - messages state to export. Default is hopefully reasonable. You can read more about possible values [here](https://raw.githack.com/rabbitmq/rabbitmq-management/rabbitmq_v3_6_5/priv/www/doc/stats.html).

For the latest list of suported options look [here](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/blob/master/src/prometheus_rabbitmq_exporter_config.erl).

## Metrics

### RabbitMQ Specific Metrics

#### Overview

* `rabbitmq_connections`<br />
Type: gauge.<br />
RabbitMQ Connections count.

* `rabbitmq_channels`<br />
Type: gauge.<br />
RabbitMQ Channels count.

* `rabbitmq_queues`<br />
Type: gauge.<br />
RabbitMQ Queues count.

* `rabbimq_exchanges`<br />
Type: gauge.<br />
RabbitMQ Exchanges count.

* `rabbitmq_consumers`<br />
Type: gauge.<br />
RabbitMQ Consumers count.

#### Queues

Labels: `vhost`, `queue`.

* `rabbitmq_queue_messages_ready`<br />
Type: gauge.<br />
Number of messages ready to be delivered to clients.

* `rabbitmq_queue_messages_unacknowledged`<br />
Type: gauge.<br />
Number of messages delivered to client but not yet acknowledged.

* `rabbitmq_queue_messages`<br />
Type: gauge.<br />
Sum of ready and unacknowledged messages (queue depth).

* `rabbitmq_queue_messages_ready_ram`<br />
Type: gauge.<br />
Number of messages from messages_ready which are resident in ram.

* `rabbitmq_queue_messages_unacknowledged_ram`<br />
Type: gauge.<br />
Number of messages from messages_unacknowledged which are resident in ram.

* `rabbitmq_queue_messages_ram`<br />
Type: gauge.<br />
Total number of messages which are resident in ram.

* `rabbitmq_queue_messages_persistent`<br />
Type: gauge.<br />
Total number of persisted messages in the queue (will always be 0 for transient queues).

* `rabbitmq_queue_message_bytes`<br />
Type: gauge.<br />
Sum of the size of all message bodies in the queue. This does not include the message properties (including headers) or any overhead.

* `rabbitmq_queue_message_bytes_ready`<br />
Type: gauge.<br />
Like message_bytes but counting only those messages ready to be delivered to clients.

* `rabbitmq_queue_message_bytes_unacknowledged`<br />
Type: gauge.<br />
Like message_bytes but counting only those messages delivered to clients but not yet acknowledged.

* `rabbitmq_queue_messages_bytes_ram`<br />
Type: gauge.<br />
Like message_bytes but counting only those messages which are in RAM.

* `rabbitmq_queue_message_bytes_persistent`<br />
Type: gauge.<br />
Like message_bytes but counting only those messages which are persistent.

* `rabbitmq_queue_messages_published_total`<br />
Type: counter.<br />
Count of messages published.

* `rabbitmq_queue_messages_confirmed_total`<br />
Type: counter.<br />
Count of messages confirmed.

* `rabbitmq_queue_messages_delivered_total`<br />
Type: counter.<br />
Count of messages delivered in acknowledgement mode to consumers.

* `rabbitmq_queue_messages_delivered_no_ack_total`<br />
Type: counter.<br />
Count of messages delivered in no-acknowledgement mode to consumers.

* `rabbitmq_queue_messages_get_total`<br />
Type: counter.<br />
Count of messages delivered in acknowledgement mode in response to basic.get.

* `rabbitmq_queue_messages_get_no_ack_total`<br />
Type: counter.<br />
Count of messages delivered in no-acknowledgement mode in response to basic.get.

* `rabbitmq_queue_messages_deliver_get_total`<br />
Type: counter.<br />
Sum of messages_delivered_total, messages_delivered_no_ack_total, messages_get_total and messages_get_no_ack_total.

* `rabbitmq_queue_messages_redelivered_total`<br />
Type: counter.<br />
Count of subset of delivered messages which had the redelivered flag set.

* `rabbitmq_queue_messages_returned_total`<br />
Type: counter.<br />
Count of messages returned to publisher as unroutable.

* `rabbitmq_queue_consumers`<br />
Type: gauge.<br />
Number of consumers.

* `rabbitmq_queue_consumer_utilization`<br />
Type: gauge.<br />
Fraction of the time (between 0.0 and 1.0) that the queue is able to immediately deliver messages to consumers. This can be less than 1.0 if consumers are limited by network congestion or prefetch count.

* `rabbitmq_queue_memory`<br />
Type: gauge.<br />
Bytes of memory consumed by the Erlang process associated with the queue, including stack, heap and internal structures.

* `rabbitmq_queue_disk_reads`<br />
Type: counter.<br />
Total number of times messages have been read from disk by this queue since it started.

* `rabbitmq_queue_disk_writes`<br />
Type: counter.<br />
Total number of times messages have been written to disk by this queue since it started.

#### Exchanges

Labels: `vhost`, `exchange`.

* `rabbitmq_exchange_messages_published_total`<br />
Type: counter.<br />
Count of messages published.

* `rabbitmq_exchange_messages_published_in_total`<br />
Type: counter.<br />
Count of messages published \"in\" to an exchange, i.e. not taking account of routing.

* `rabbitmq_exchange_messages_published_out_total`<br />
Type: counter.<br />
Count of messages published \"out\" of an exchange, i.e. taking account of routing.

* `rabbitmq_exchange_messages_confirmed_total`<br />
Type: counter.<br />
Count of messages confirmed.

* `rabbitmq_exchange_messages_delivered_total`<br />
Type: counter.<br />
Count of messages delivered in acknowledgement mode to consumers.

* `rabbitmq_exchange_messages_delivered_no_ack_total`<br />
Type: counter.<br />
Count of messages delivered in no-acknowledgement mode to consumers.

* `rabbitmq_exchange_messages_get_total`<br />
Type: counter.<br />
Count of messages delivered in acknowledgement mode in response to basic.get.

* `rabbitmq_exchange_messages_get_no_ack_total`<br />
Type: counter.<br />
Count of messages delivered in no-acknowledgement mode in response to basic.get.

* `rabbitmq_exchange_messages_redelivered_total`<br />
Type: counter.<br />
Count of subset of delivered messages which had the redelivered flag set.

* `rabbitmq_exchange_messages_returned_total`<br />
Type: counter.<br />
Count of messages returned to publisher as unroutable.

### Erlang VM Metrics

#### System Info

* `erlang_vm_ets_limit`<br />
Type: gauge.<br />
The maximum number of ETS tables allowed.

* `erlang_vm_logical_processors`<br />
Type: gauge.<br />
The detected number of logical processors configured in the system.

* `erlang_vm_logical_processors_available`<br />
Type: gauge.<br />
The detected number of logical processors
available to the Erlang runtime system.

* `erlang_vm_logical_processors_online`<br />
Type: gauge.<br />
The detected number of logical processors online on the system.

* `erlang_vm_port_count`<br />
Type: gauge.<br />
The number of ports currently existing at the local node.

* `erlang_vm_port_limit`<br />
Type: gauge.<br />
The maximum number of simultaneously existing ports at the local node.

* `erlang_vm_process_count`<br />
Type: gauge.<br />
The number of processes currently existing at the local node.

* `erlang_vm_process_limit`<br />
Type: gauge.<br />
The maximum number of simultaneously existing processes
at the local node.

* `erlang_vm_schedulers`<br />
Type: gauge.<br />
The number of scheduler threads used by the emulator.

* `erlang_vm_schedulers_online`<br />
Type: gauge.<br />
The number of schedulers online.

* `erlang_vm_smp_support`<br />
Type: boolean.<br />
1 if the emulator has been compiled with SMP support, otherwise 0.

* `erlang_vm_threads`<br />
Type: boolean.<br />
1 if the emulator has been compiled with thread support, otherwise 0.

* `erlang_vm_thread_pool_size`<br />
Type: gauge.<br />
The number of async threads in the async thread pool
used for asynchronous driver calls.

* `erlang_vm_time_correction`<br />
Type: boolean.<br />
1 if time correction is enabled, otherwise 0.

#### Statistics

* `erlang_vm_statistics_bytes_output_total`<br />
Type: counter.<br />
The total number of bytes output to ports.

* `erlang_vm_statistics_bytes_received_total`<br />
Type: counter.<br />
The total number of bytes received through ports.

* `erlang_vm_statistics_context_switches`<br />
Type: counter.<br />
The total number of context switches since the system started.

* `erlang_vm_statistics_garbage_collection_number_of_gcs`<br />
Type: counter.<br />
The total number of garbage collections since the system started.

* `erlang_vm_statistics_garbage_collection_words_reclaimed`<br />
Type: counter.<br />
The total number of words reclaimed by GC since the system started.

* `erlang_vm_statistics_reductions_total`<br />
Type: counter.<br />
Total reductions count.

* `erlang_vm_statistics_run_queues_length_total`<br />
Type: gauge.<br />
The total length of the run-queues. That is, the number of
processes and ports that are ready to run on all available run-queues.

* `erlang_vm_statistics_runtime_milliseconds`<br />
Type: counter.<br />
The sum of the runtime for all threads in the Erlang runtime system.

* `erlang_vm_statistics_wallclock_time_milliseconds`<br />
Type: counter.<br />
Can be used in the same manner as
`erlang_vm_statistics_runtime_milliseconds`, except that real time is
measured as opposed to runtime or CPU time.

#### Memory

* `erlang_vm_memory_atom_bytes_total{usage="free|used"}`<br />
Type: gauge.<br />
The total amount of memory currently allocated for atoms.
This memory is part of the memory presented as system memory.

* `erlang_vm_memory_bytes_total{kind="system|processes"}`<br />
Type: gauge.<br />
The total amount of memory currently allocated.
This is the same as the sum of the memory size for processes and system.

* `erlang_vm_dets_tables`<br />
Type: gauge.<br />
Erlang VM DETS Tables count.

* `erlang_vm_ets_tables`<br />
Type: gauge.<br />
Erlang VM ETS Tables count.

* `erlang_vm_memory_processes_bytes_total{usage="free|used"}`<br />
Type: gauge.<br />
The total amount of memory currently allocated for the Erlang processes.

* `erlang_vm_memory_system_bytes_total{usage="atom|binary|code|ets|other"}`<br />
Type: gauge.<br />
The total amount of memory currently allocated for the emulator
that is not directly related to any Erlang process.
Memory presented as processes is not included in this memory.


### Process Metrics
(Process info collector must be enabled)

* `process_open_fds`<br />
Type: gauge.<br />
Number of open file descriptors.

* `process_max_fds`<br />
Type: gauge.<br />
Maximum number of open file descriptors.

* `process_start_time_seconds`<br />
Type: gauge.<br />
Start time of the process since unix epoch in seconds.

* `process_uptime_seconds`<br />
Type: gauge.<br />
Process uptime in seconds.

* `process_threads_total`<br />
Type: gauge.<br />
Process threads count.

* `process_virtual_memory_bytes`<br />
Type: gauge.<br />
Virtual memory size in bytes.

* `process_resident_memory_bytes`<br />
Type: gauge.<br /> 
Resident memory size in bytes;

* `process_cpu_seconds_total{kind="utime|stime"}`<br />
Type: counter.<br />
Process CPU time.

## License
MIT
