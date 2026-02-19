/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {GatewayRpcMethodError} from '@fluxer/api/src/infrastructure/GatewayRpcError';
import type {IGatewayRpcTransport} from '@fluxer/api/src/infrastructure/IGatewayRpcTransport';
import {vi} from 'vitest';

export class MockGatewayRpcTransport implements IGatewayRpcTransport {
	private methodErrors = new Map<string, string>();
	private methodResults = new Map<string, unknown>();

	call = vi.fn(async (method: string, _params: Record<string, unknown>): Promise<unknown> => {
		const errorCode = this.methodErrors.get(method);
		if (errorCode !== undefined) {
			throw new GatewayRpcMethodError(errorCode);
		}

		const result = this.methodResults.get(method);
		if (result !== undefined) {
			return result;
		}

		return {};
	});

	destroy = vi.fn(async (): Promise<void> => {});

	setMethodError(method: string, errorCode: string): void {
		this.methodErrors.set(method, errorCode);
		this.methodResults.delete(method);
	}

	setMethodResult(method: string, result: unknown): void {
		this.methodResults.set(method, result);
		this.methodErrors.delete(method);
	}

	reset(): void {
		this.methodErrors.clear();
		this.methodResults.clear();
		this.call.mockClear();
		this.destroy.mockClear();
	}
}
