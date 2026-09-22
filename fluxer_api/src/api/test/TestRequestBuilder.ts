// SPDX-License-Identifier: AGPL-3.0-or-later

import {drainSearchTasks} from '@app/api/search/SearchTaskTracker';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';

export class TestRequestBuilder<TResponse = unknown> {
	private path: string = '';
	private method: string = 'GET';
	private requestBody: unknown = undefined;
	private headers: Record<string, string> = {};
	private allowedStatuses: Set<number> = new Set([200]);
	private expectedErrorCode: string | null = null;

	constructor(
		private harness: ApiTestHarness,
		token: string,
	) {
		if (token) {
			this.headers.Authorization = token;
		}
	}

	get(path: string): this {
		this.path = path;
		this.method = 'GET';
		return this;
	}

	post(path: string): this {
		this.path = path;
		this.method = 'POST';
		return this;
	}

	put(path: string): this {
		this.path = path;
		this.method = 'PUT';
		return this;
	}

	delete(path: string): this {
		this.path = path;
		this.method = 'DELETE';
		return this;
	}

	patch(path: string): this {
		this.path = path;
		this.method = 'PATCH';
		return this;
	}

	body<T>(data: T): this {
		this.requestBody = data;
		return this;
	}

	expect(status: number, errorCode?: string): this {
		this.allowedStatuses = new Set([status]);
		this.expectedErrorCode = errorCode ?? null;
		return this;
	}

	header(key: string, value: string): this {
		this.headers[key] = value;
		return this;
	}

	async execute(): Promise<TResponse> {
		const response = await this.harness.requestJson({
			path: this.path,
			method: this.method,
			body: this.requestBody,
			headers: this.headers,
		});
		try {
			if (!this.allowedStatuses.has(response.status)) {
				const text = await response.text();
				const expected = Array.from(this.allowedStatuses).join(', ');
				throw new Error(`Expected ${expected}, got ${response.status}: ${text}`);
			}
			const text = await response.text();
			if (text.length === 0) {
				return undefined as TResponse;
			}
			let json: TResponse;
			try {
				json = JSON.parse(text) as TResponse;
			} catch {
				throw new Error(`Failed to parse response as JSON: ${text}`);
			}
			if (this.expectedErrorCode !== null) {
				const actualCode = (
					json as {
						code?: string;
					}
				).code;
				if (actualCode !== this.expectedErrorCode) {
					throw new Error(`Expected error code '${this.expectedErrorCode}', got '${actualCode}'`);
				}
			}
			return json;
		} finally {
			await drainSearchTasks();
		}
	}

	async executeWithResponse(): Promise<{
		response: Response;
		json: TResponse;
	}> {
		const response = await this.harness.requestJson({
			path: this.path,
			method: this.method,
			body: this.requestBody,
			headers: this.headers,
		});
		try {
			if (!this.allowedStatuses.has(response.status)) {
				const text = await response.text();
				const expected = Array.from(this.allowedStatuses).join(', ');
				throw new Error(`Expected ${expected}, got ${response.status}: ${text}`);
			}
			const text = await response.text();
			let json: TResponse;
			if (text.length === 0) {
				json = undefined as TResponse;
			} else {
				try {
					json = JSON.parse(text) as TResponse;
				} catch {
					throw new Error(`Failed to parse response as JSON: ${text}`);
				}
			}
			return {response, json};
		} finally {
			await drainSearchTasks();
		}
	}

	async executeRaw(): Promise<{
		response: Response;
		text: string;
		json: TResponse;
	}> {
		const response = await this.harness.requestJson({
			path: this.path,
			method: this.method,
			body: this.requestBody,
			headers: this.headers,
		});
		try {
			const text = await response.text();
			let json: TResponse = undefined as TResponse;
			if (text.length > 0) {
				try {
					json = JSON.parse(text) as TResponse;
				} catch {}
			}
			return {response, text, json};
		} finally {
			await drainSearchTasks();
		}
	}
}

export function createBuilder<TResponse = unknown>(
	harness: ApiTestHarness,
	token: string,
): TestRequestBuilder<TResponse> {
	return new TestRequestBuilder<TResponse>(harness, token);
}

export function createBuilderWithoutAuth<TResponse = unknown>(harness: ApiTestHarness): TestRequestBuilder<TResponse> {
	return new TestRequestBuilder<TResponse>(harness, '');
}
