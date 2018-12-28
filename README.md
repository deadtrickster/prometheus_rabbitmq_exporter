# RabbitMQ Prometheus.io exporter

Implemented as RabbitMQ Management Plugin plugin.
Also exports Erlang VM and process metrics (~ 100 metrics in total).

Implemented using [Erlang Prometheus.io client](https://github.com/deadtrickster/prometheus.erl)

![rabbitmq prometheus exporter grafana dashboard](http://i.imgur.com/tWiDw56.png?1)

## TOC
 - [Versioning](#versioning)
 - [Installation](#installation)
   - [Troubleshooting](#troubleshooting)
 - [Configuration](#configuration)
 - [Metrics](#metrics)
   - [RabbitMQ specific metrics](#rabbitmq-specific-metrics)
     - [Overview](#overview)
     - [Queues](#queues)
     - [Exchanges](#exchanges)
	 - [Mnesia tables](#mnesia-tables)
	 - [Nodes](#nodes)
   - [Erlang VM & OTP metrics](#erlang-vm--otp-metrics)
     - [System Info](#system-info)
     - [Statistics](#statistics)
     - [Memory](#memory)
     - [Mnesia](#mnesia)
   - [Process metrics](#process-metrics)
   - [Exporter metrics](#exporter-metrics)
 - [License](#license)

## Versioning

While RabbitMQ transitions from webmachine to cowboy we maintain two branches one for 3.6.x and one for 3.7.x.
Plugin version should be read as follows: 3.7.1.x - where 3.7.1 is required RabbitMQ version and x is just incremental version of the plugin.

## Installation

 - [Release for latest RabbitMQ 3.7.x](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/v3.7.2.3)
 - [Release for latest RabbitMQ 3.6.x](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.6.14.1);
 - [Release for RabbitMQ 3.6.8 and 3.6.9](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.6.9.1);
 - [Release for RabbitMQ 3.6.5](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.6.5.9).

Download suitable version and follow regular [RabbitMQ plugin installation instructions](http://www.rabbitmq.com/installing-plugins.html).

```
 rabbitmq-plugins enable prometheus_rabbitmq_exporter
```

If you are running on Linux/FreeBSD/Mac, you may find `prometheus_process_exporter` useful:

```
rabbitmq-plugins enable prometheus_process_collector
```

[Prometheus process collector](https://github.com/deadtrickster/prometheus_process_collector).

### Troubleshooting

#### `undef` error

If you see something like this:

```
{could_not_start,rabbitmq_management,
       {undef,
           [{prometheus_http,setup,[],[]}
```

I.e. `undef` error mentioning a module starting with `prometheus_`, chances you forgot to enable a plugin (see https://github.com/deadtrickster/prometheus_rabbitmq_exporter/issues/27 for example).

#### Module `prometheus_process_collector` is unloadable

```
{plugin_module_unloadable,"prometheus_process_collector",
                             {error,on_load_failure}}
```

or

```
{error,{load_failed,"Failed to load NIF library:
 '/<...>/plugins/prometheus_process_collector-1.1.0/priv/prometheus_process_collector.so:
 failed to map segment from shared object'"}}
```

Prometheus process collector uses NIFs underneath and failed to load shared object in module on_load callback.
Please check that `RABBITMQ_PLUGINS_EXPAND_DIR` doesn't have `noexec` flag set (see https://github.com/deadtrickster/prometheus_rabbitmq_exporter/issues/26).

#### Crashes with something like {error,{bad_lib,"Library version (2.11) not compatible (with 2.10)."}}

This means [erl_nif](http://erlang.org/doc/man/erl_nif.html) version `prometheus_process_collector` built with differs from yours (see Version Management section).
You can rebuild the plugin yourself very easily - `clone https://github.com/deadtrickster/prometheus_process_collector.git` and run `rebar3 archive`

#### Glibc-related errors when `prometheus_process_collector` enabled

`prometheus_process_collector` plugin comes with prebuilt shared object. And it looks like my Glibc version differs from yours.
You can rebuild the plugin yourself very easily - `clone https://github.com/deadtrickster/prometheus_process_collector.git` and run `rebar3 archive`

### Latest Docker:
 `docker run -p 8080:15672 deadtrickster/rabbitmq_prometheus`

Alpine-based image is also available:
 `docker run -p 8080:15672 deadtrickster/rabbitmq_prometheus:latest-alpine`

## Configuration

This exporter supports the following options via `rabbitmq_exporter` entry of `prometheus` app env:
 - `path` - scrape endpoint. Default is `"metrics"`. Note RabbitMQ translates this to `"{management_plugin_path_prefix}/api/metrics"`;
 - `use_mgmt_auth` - use built-in management auth. Default is `false`. If true, relies on management plugin for authentication (that guest:guest on fresh setups);
 - `format` - scrape format. Default is `prometheus_text_format`;
 - `exchange_messages_stat` - same as `queue_messages_state` but for the exchanges;
 - `queue_messages_stat` - messages state to export. Default is hopefully reasonable. You can read more about possible values [here](https://raw.githack.com/rabbitmq/rabbitmq-management/rabbitmq_v3_6_5/priv/www/doc/stats.html);
 - `connections_total_enabled` - Default is `false`. If `true`, the exporter will iterate over all connections and export count grouped by connection state (running, flow, etc).

Sample `/etc/rabbitmq/rabbitmq.config` showing how to customize the scrape `path`, and `connections_total_enabled`:

```erlang
[
 {rabbit, [
   {loopback_users, []},
   {tcp_listeners, [5672]},
   {ssl_listeners, []}
 ]},
 {prometheus, [
   {rabbitmq_exporter, [
     {path, "/mymetrics"},
     {connections_total_enabled, true}
   ]}
 ]},
 {rabbitmq_management, [
   {listener, [
     {port, 15672},
     {ssl, false}
   ]}
 ]}
].
```

For the latest list of supported options look [here](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/blob/master/src/prometheus_rabbitmq_exporter_config.erl).

## Metrics

### RabbitMQ Specific Metrics

#### Overview

* `rabbitmq_connections`<br />
Type: gauge.<br />
RabbitMQ Connections count.

* `rabbitmq_connections_total` (disabled by default)<br />
Type: gauge.<br />
Labels: state.<br />
RabbitMQ connections count grouped by connection state.

* `rabbitmq_channels`<br />
Type: gauge.<br />
RabbitMQ Channels count.

* `rabbitmq_queues`<br />
Type: gauge.<br />
RabbitMQ Queues count.

* `rabbitmq_exchanges`<br />
Type: gauge.<br />
RabbitMQ Exchanges count.

* `rabbitmq_consumers`<br />
Type: gauge.<br />
RabbitMQ Consumers count.

* `rabbitmq_queues_disk_reads`<br />
Type: counter.<br />
Total number of times messages have been read from disk by all queues.

* `rabbitmq_queues_disk_writes`<br />
Type: counter.<br />
Total number of times messages have been written to disk by all queues.

* `rabbitmq_messages_ready`<br />
Type: gauge.<br />
Messages ready for delivery.

* `rabbitmq_messages_unacknowledged`<br />
Type: gauge.<br />
Delivered but unacknowledged messages.

* `rabbitmq_messages_published_total`<br />
Type: counter.<br />
Count of messages published.

* `rabbitmq_messages_confirmed_total`<br />
Type: counter.<br />
Count of messages confirmed.

* `rabbitmq_messages_delivered_total`<br />
Type: counter.<br />
Count of messages delivered in acknowledgement mode to consumers.

* `rabbitmq_messages_delivered_no_ack_total`<br />
Type: counter.<br />
Count of messages delivered in no-acknowledgement mode to consumers.

* `rabbitmq_messages_get_total`<br />
Type: counter.<br />
Count of messages delivered in acknowledgement mode in response to basic.get.

* `rabbitmq_messages_get_no_ack_total`<br />
Type: counter.<br />
Count of messages delivered in no-acknowledgement mode in response to basic.get.

* `rabbitmq_messages_deliver_get_total`<br />
Type: counter.<br />
Sum of messages_delivered_total, messages_delivered_no_ack_total, messages_get_total and messages_get_no_ack_total.

* `rabbitmq_messages_redelivered_total`<br />
Type: counter.<br />
Count of subset of delivered messages which had the redelivered flag set.

* `rabbitmq_messages_returned_total`<br />
Type: counter.<br />
Count of messages returned to publisher as unroutable.

#### Queues

Labels: `vhost`, `queue`.

* `rabbitmq_queue_durable`<br />
Type: boolean.<br />
Whether or not the queue survives server restarts.

* `rabbitmq_queue_auto_delete`<br />
Type: boolean.<br />
Whether the queue will be deleted automatically when no longer used.

* `rabbitmq_queue_exclusive`<br />
Type: boolean.<br />
True if queue is exclusive (i.e. has owner_pid), false otherwise.

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

* `rabbitmq_queue_message_bytes_ram`<br />
Type: gauge.<br />
Like message_bytes but counting only those messages which are in RAM.

* `rabbitmq_queue_message_bytes_persistent`<br />
Type: gauge.<br />
Like message_bytes but counting only those messages which are persistent.

* `rabbitmq_queue_head_message_timestamp`<br />
Type: gauge.<br />
The timestamp property of the first message in the queue, if present. Timestamps of messages only appear when they are in the paged-in state.

* `rabbitmq_queue_disk_reads`<br />
Type: counter.<br />
Total number of times messages have been read from disk by this queue since it started.

* `rabbitmq_queue_disk_writes`<br />
Type: counter.<br />
Total number of times messages have been written to disk by this queue since it started.

* `rabbitmq_queue_disk_size_bytes`<br />
Type: gauge.<br />
Disk space occupied by the queue.

* `rabbitmq_queue_consumers`<br />
Type: gauge.<br />
Number of consumers.

* `rabbitmq_queue_consumer_utilisation`<br />
Type: gauge.<br />
Fraction of the time (between 0.0 and 1.0) that the queue is able to immediately deliver messages to consumers. This can be less than 1.0 if consumers are limited by network congestion or prefetch count.

* `rabbitmq_queue_memory`<br />
Type: gauge.<br />
Bytes of memory consumed by the Erlang process associated with the queue, including stack, heap and internal structures.

* `rabbitmq_queue_state`<br />
Type: gauge.<br />
The state of the queue. NaN if queue is located on cluster nodes that are currently down. 0 if queue is running normally. MsgCount if queue is synchronising.

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

* `rabbitmq_exchange_messages_deliver_get_total`<br />
Type: counter.<br />
Sum of *messages_delivered_total, *messages_delivered_no_ack_total, *messages_get_total and *messages_get_no_ack_total.

* `rabbitmq_exchange_messages_redelivered_total`<br />
Type: counter.<br />
Count of subset of delivered messages which had the redelivered flag set.

* `rabbitmq_exchange_messages_returned_total`<br />
Type: counter.<br />
Count of messages returned to publisher as unroutable.

#### Mnesia Tables

Various metrics for RabbitMQ-specific Mnesia tables.

Labels: `table`.

* `rabbitmq_mnesia_table_read_only`<br />
Type: boolean.<br />
Access mode of the table, 1 if table is read_only or 0 otherwise.

* `rabbitmq_mnesia_table_disc_copies`<br />
Type: gauge.<br />
Number of the nodes where a disc_copy of the table resides according to the schema.

* `rabbitmq_mnesia_table_disc_only_copies`<br />
Type: gauge.<br />
Number of the nodes where a disc_only_copy of the table resides according to the schema.

* `rabbitmq_mnesia_table_local_content`<br />
Type: boolean.<br />
If the table is configured to have locally unique content on each node, value is 1 or 0 otherwise.

* `rabbitmq_mnesia_table_majority_required`<br />
Type: boolean.<br />
If 1, a majority of the table replicas must be available for an update to succeed.

* `rabbitmq_mnesia_table_master_nodes`<br />
Type: gauge.<br />
Number of the master nodes of a table.

* `rabbitmq_mnesia_table_memory_bytes`<br />
Type: gauge.<br />
The number of bytes allocated to the table on this node.

* `rabbitmq_mnesia_table_ram_copies`<br />
Type: gauge.<br />
Number of the nodes where a ram_copy of the table resides according to the schema.

* `rabbitmq_mnesia_table_records_count`<br />
Type: gauge.<br />
Number of records inserted in the table.

* `rabbitmq_mnesia_table_disk_size_bytes`<br />
Type: gauge.<br />
Disk space occupied by the table (DCL + DCD).

#### Nodes

Cluster/nodes metrics.

* `rabbitmq_node_up`<br />
Type: boolean.<br />
Labels: name, type.<br />
Node running status.

### Erlang VM & OTP Metrics

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

* `erlang_vm_statistics_garbage_collection_bytes_reclaimed`<br />
Type: counter.<br />
The total number of bytes reclaimed by GC since the system started.

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

### Mnesia

* `erlang_mnesia_held_locks`<br />
Type: gauge.<br />
Number of held locks.

* `erlang_mnesia_lock_queue`<br />
Type: gauge.<br />
Number of transactions waiting for a lock.

* `erlang_mnesia_transaction_participants`<br />
Type: gauge.<br />
Number of participant transactions.

* `erlang_mnesia_transaction_coordinators`<br />
Type: gauge.<br />
Number of coordinator transactions.

* `erlang_mnesia_failed_transactions`<br />
Type: counter.<br />
Number of failed (i.e. aborted) transactions.

* `erlang_mnesia_committed_transactions`<br />
Type: gauge.<br />
Number of committed transactions.

* `erlang_mnesia_logged_transactions`<br />
Type: counter.<br />
Number of transactions logged.

* `erlang_mnesia_restarted_transactions`<br />
Type: counter.<br />
Total number of transaction restarts.

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

### Exporter Metrics

Labels: `registry`, `content_type`.

* `telemetry_scrape_duration_seconds`<br />
Type: summary.<br />
Scrape duration.

* `telemetry_scrape_size_bytes`<br />
Type: summary.<br />
Scrape size, uncompressed.

## License
MIT
