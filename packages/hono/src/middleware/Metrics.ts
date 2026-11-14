// SPDX-License-Identifier: AGPL-3.0-or-later

import {isLoopbackIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {HttpBindings} from '@hono/node-server';
import type {Context, Handler, MiddlewareHandler} from 'hono';

const SKIP_PATHS = new Set(['/_health', '/_healthz', '/_metrics']);

function isLoopbackPeer(env: unknown): boolean {
	const remoteAddress = (env as Partial<HttpBindings> | null | undefined)?.incoming?.socket?.remoteAddress;
	if (typeof remoteAddress !== 'string' || remoteAddress === '') {
		return false;
	}
	return isLoopbackIpAddress(remoteAddress);
}

const DEFAULT_BUCKETS = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10];

function statusClass(status: number): string {
	if (status < 200) return '1xx';
	if (status < 300) return '2xx';
	if (status < 400) return '3xx';
	if (status < 500) return '4xx';
	return '5xx';
}

export class Counter {
	private labels = new Map<string, number>();

	inc(labelKey: string = '', amount = 1): void {
		this.labels.set(labelKey, (this.labels.get(labelKey) ?? 0) + amount);
	}

	render(name: string, help: string): string {
		const lines: Array<string> = [];
		lines.push(`# HELP ${name} ${help}`);
		lines.push(`# TYPE ${name} counter`);
		if (this.labels.size === 0) {
			lines.push(`${name} 0`);
		} else {
			for (const [key, value] of this.labels) {
				if (key === '') {
					lines.push(`${name} ${value}`);
				} else {
					lines.push(`${name}{${key}} ${value}`);
				}
			}
		}
		return lines.join('\n');
	}
}

export class Histogram {
	private readonly buckets: Array<number>;
	private readonly counts: Array<number>;
	private sum = 0;
	private count = 0;

	constructor(buckets: Array<number>) {
		this.buckets = [...buckets].sort((a, b) => a - b);
		this.counts = new Array<number>(this.buckets.length).fill(0);
	}

	observe(value: number): void {
		this.sum += value;
		this.count += 1;
		for (let i = 0; i < this.buckets.length; i++) {
			if (value <= this.buckets[i]) {
				this.counts[i] += 1;
			}
		}
	}

	render(name: string, help: string): string {
		const lines: Array<string> = [];
		lines.push(`# HELP ${name} ${help}`);
		lines.push(`# TYPE ${name} histogram`);
		for (let i = 0; i < this.buckets.length; i++) {
			lines.push(`${name}_bucket{le="${this.buckets[i]}"} ${this.counts[i]}`);
		}
		lines.push(`${name}_bucket{le="+Inf"} ${this.count}`);
		lines.push(`${name}_sum ${this.sum}`);
		lines.push(`${name}_count ${this.count}`);
		return lines.join('\n');
	}
}

export class Gauge {
	private valueFn: () => number;

	constructor(valueFn: () => number) {
		this.valueFn = valueFn;
	}

	render(name: string, help: string): string {
		const lines: Array<string> = [];
		lines.push(`# HELP ${name} ${help}`);
		lines.push(`# TYPE ${name} gauge`);
		lines.push(`${name} ${this.valueFn()}`);
		return lines.join('\n');
	}
}

const registeredSections = new Set<() => string>();

export function registerMetricsSection(render: () => string): () => void {
	registeredSections.add(render);
	return () => {
		registeredSections.delete(render);
	};
}

function renderRegisteredSections(): Array<string> {
	const rendered: Array<string> = [];
	for (const render of registeredSections) {
		const text = render().trim();
		if (text !== '') rendered.push(text);
	}
	return rendered;
}

interface RegisteredMetric {
	help: string;
	metric: Counter | Histogram | Gauge;
}

const registeredMetrics = new Map<string, RegisteredMetric>();

function register<T extends Counter | Histogram | Gauge>(
	name: string,
	help: string,
	create: () => T,
	kind: new (...args: Array<never>) => T,
): T {
	const existing = registeredMetrics.get(name);
	if (existing !== undefined) {
		if (!(existing.metric instanceof kind)) {
			throw new Error(`metric ${name} is already registered with another type`);
		}
		return existing.metric;
	}
	const metric = create();
	registeredMetrics.set(name, {help, metric});
	return metric;
}

export function registerCounter(name: string, help: string): Counter {
	return register(name, help, () => new Counter(), Counter);
}

export function registerHistogram(name: string, help: string, buckets: Array<number> = DEFAULT_BUCKETS): Histogram {
	return register(name, help, () => new Histogram(buckets), Histogram);
}

export function registerGauge(name: string, help: string, valueFn: () => number): Gauge {
	return register(name, help, () => new Gauge(valueFn), Gauge);
}

function renderRegisteredMetrics(): Array<string> {
	return [...registeredMetrics].map(([name, {help, metric}]) => metric.render(name, help));
}

const METRICS_CONTENT_TYPE = 'text/plain; version=0.0.4; charset=utf-8';

function renderMetricsResponse(c: Context, sections: Array<string>): Response {
	if (!isLoopbackPeer(c.env)) {
		return c.text('FORBIDDEN', 403, {'Content-Type': 'text/plain'});
	}
	return c.text(`${[...sections, ...renderRegisteredMetrics()].join('\n\n')}\n`, 200, {
		'Content-Type': METRICS_CONTENT_TYPE,
	});
}

export function createRegisteredMetricsHandler(): Handler {
	return (c) => renderMetricsResponse(c, []);
}

interface MetricsState {
	requestsTotal: Counter;
	errorsTotal: Counter;
	requestDuration: Histogram;
	uptime: Gauge;
}

interface MetricsResult {
	middleware: MiddlewareHandler;
	metricsHandler: Handler;
	state: MetricsState;
}

export function createMetricsMiddleware(serviceName: string): MetricsResult {
	const prefix = `fluxer_${serviceName}`;
	const startTime = performance.now();

	const requestsTotal = new Counter();
	const errorsTotal = new Counter();
	const requestDuration = new Histogram(DEFAULT_BUCKETS);
	const uptime = new Gauge(() => (performance.now() - startTime) / 1000);

	const state: MetricsState = {requestsTotal, errorsTotal, requestDuration, uptime};

	const middleware: MiddlewareHandler = async (c, next) => {
		if (SKIP_PATHS.has(c.req.path)) {
			await next();
			return;
		}

		const start = performance.now();
		await next();
		const durationSeconds = (performance.now() - start) / 1000;

		const method = c.req.method;
		const status = c.res.status;
		const cls = statusClass(status);

		requestsTotal.inc(`method="${method}",status="${cls}"`);
		requestDuration.observe(durationSeconds);

		if (status >= 500) {
			errorsTotal.inc(`method="${method}"`);
		}
	};

	const metricsHandler: Handler = (c) =>
		renderMetricsResponse(c, [
			requestsTotal.render(`${prefix}_http_requests_total`, 'Total HTTP requests'),
			requestDuration.render(`${prefix}_http_request_duration_seconds`, 'HTTP request duration in seconds'),
			errorsTotal.render(`${prefix}_http_errors_total`, 'Total HTTP 5xx errors'),
			uptime.render(`${prefix}_uptime_seconds`, 'Process uptime in seconds'),
			...renderRegisteredSections(),
		]);

	return {middleware, metricsHandler, state};
}
