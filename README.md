#RabbitMQ Prometheus.io exporter

Implemented as RabbitMQ Management Plugin plugin.
Also exports ErlangVM metrics.

Implemented using [Erlang Prometheus.io client](https://github.com/deadtrickster/prometheus.erl)

![rabbitmq prometheus exporter grafana dashboard](http://i.imgur.com/tWiDw56.png?1)

## Versioning

While RabbitMQ transitions from webmachine to cowboy we maintain two branches one for 3.6.x and one for 3.7.x.
Plugin version should be read as follows: 3.7.1.x - where 3.7.1 is required RabbitMQ version and x is just incremental version of the plugin.

## Installation

 - [Release for RabbitMQ 3.6.5](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.6.5.1)
 - [Release for RabbitMQ 3.7](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/releases/tag/rabbitmq-3.7_1)

Download suitable version and follow regular [RabbitMQ plugin installation instructions](http://www.rabbitmq.com/installing-plugins.html).

Do not forget to fire `rabbitmq-plugins enable`!

## Configuration

This exporter supports the following options via `rabbitmq_exporter` entry of `prometheus` app env:
 - `path` - scrape endpoint. Default is `"metrics"`. Note RabbitMQ translates this to `"/api/metrics"`.
 - `format` - scrape format. Default is `prometheus_text_format`.
 - `queue_messages_stat` - messages state to export. Default is hopefully reasonable. You can read more about possible values [here](https://raw.githack.com/rabbitmq/rabbitmq-management/rabbitmq_v3_6_5/priv/www/doc/stats.html).

For the latest list of suported options look [here](https://github.com/deadtrickster/prometheus_rabbitmq_exporter/blob/master/src/prometheus_rabbitmq_exporter_config.erl).

## License
MIT
