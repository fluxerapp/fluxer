// SPDX-License-Identifier: AGPL-3.0-or-later

export interface MeilisearchClientConfig {
	host: string;
	apiKey?: string;
	requestTimeoutMs: number;
}

export interface MeilisearchTask {
	taskUid: number;
	status: 'enqueued' | 'processing' | 'succeeded' | 'failed' | 'canceled';
	error?: {
		message?: string;
		code?: string;
		type?: string;
		link?: string;
	};
}

export class MeilisearchTaskError extends Error {
	readonly code: string | undefined;

	constructor(message: string, code: string | undefined) {
		super(message);
		this.name = 'MeilisearchTaskError';
		this.code = code;
	}
}

export interface MeilisearchClient {
	request<TResponse>(method: string, path: string, body?: unknown): Promise<TResponse>;
	waitForTask(taskUid: number): Promise<void>;
}

const TASK_POLL_INTERVAL_MS = 50;
const TASK_TIMEOUT_MS = 30_000;

function trimTrailingSlash(url: string): string {
	return url.replace(/\/+$/u, '');
}

function isAbortError(error: unknown): boolean {
	return error instanceof Error && error.name === 'AbortError';
}

export class MeilisearchHttpClient implements MeilisearchClient {
	private readonly host: string;
	private readonly apiKey: string | undefined;
	private readonly requestTimeoutMs: number;

	constructor(config: MeilisearchClientConfig) {
		this.host = trimTrailingSlash(config.host);
		this.apiKey = config.apiKey || undefined;
		this.requestTimeoutMs = config.requestTimeoutMs;
	}

	async request<TResponse>(method: string, path: string, body?: unknown): Promise<TResponse> {
		const controller = new AbortController();
		const timeout = setTimeout(() => controller.abort(), this.requestTimeoutMs);
		try {
			const response = await fetch(`${this.host}${path}`, {
				method,
				headers: {
					Accept: 'application/json',
					...(body === undefined ? {} : {'Content-Type': 'application/json'}),
					...(this.apiKey ? {Authorization: `Bearer ${this.apiKey}`} : {}),
				},
				body: body === undefined ? undefined : JSON.stringify(body),
				signal: controller.signal,
			});
			if (!response.ok) {
				const responseBody = await response.text();
				throw new Error(`Meilisearch ${method} ${path} failed with ${response.status}: ${responseBody}`);
			}
			if (response.status === 204) {
				return undefined as TResponse;
			}
			return (await response.json()) as TResponse;
		} catch (error) {
			if (isAbortError(error)) {
				throw new Error(`Meilisearch ${method} ${path} timed out after ${this.requestTimeoutMs}ms`);
			}
			throw error;
		} finally {
			clearTimeout(timeout);
		}
	}

	async waitForTask(taskUid: number): Promise<void> {
		const deadline = Date.now() + TASK_TIMEOUT_MS;
		while (Date.now() < deadline) {
			const task = await this.request<MeilisearchTask>('GET', `/tasks/${taskUid}`);
			if (task.status === 'succeeded') {
				return;
			}
			if (task.status === 'failed' || task.status === 'canceled') {
				throw new MeilisearchTaskError(
					task.error?.message ?? `Meilisearch task ${taskUid} ${task.status}`,
					task.error?.code,
				);
			}
			await new Promise((resolve) => setTimeout(resolve, TASK_POLL_INTERVAL_MS));
		}
		throw new Error(`Timed out waiting for Meilisearch task ${taskUid}`);
	}
}
