#RabbitMQ Prometheus.io exporter

Implemented as RabbitMQ Management Plugin plugin.
Also exports ErlangVM metrics.

Implemented using [Erlang Prometheus.io client](https://github.com/deadtrickster/prometheus.erl)

![screenshot](https://raw.githubusercontent.com/deadtrickster/prometheus_rabbitmq_exporter/master/priv/dashboards/RabbitMQErlangVM.png)

## Versioning

While RabbitMQ transitions from webmachine to cowboy we maintain two branches one for 3.6.x and one for 3.7.x.
Plugin version should be read as follows: 3.7.x - where 3.7 is minimum required RabbitMQ version and x is just incremental version of the plugin.

## Installation

 - [Release for RabbitMQ 3.6](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.6_2)
 - [Release for RabbitMQ 3.7](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.7_1)

Download suitable version and follow regular [RabbitMQ plugin installation instructions](http://www.rabbitmq.com/installing-plugins.html).

Do not forget to fire `rabbitmq-plugins enable`!

Metrics url - `server:15672/api/metrics`

## License
MIT
