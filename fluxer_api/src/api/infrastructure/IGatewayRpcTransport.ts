// SPDX-License-Identifier: AGPL-3.0-or-later

export interface IGatewayRpcTransport {
	call(method: string, params: Record<string, unknown>): Promise<unknown>;
	publish(subject: string, payload: Record<string, unknown>): Promise<void>;
	destroy(): Promise<void>;
}
