// SPDX-License-Identifier: AGPL-3.0-or-later

export interface DesktopHTTPTransportRequest {
	readonly url: string;
	readonly method: string;
	readonly headers: Readonly<Record<string, string>>;
	readonly body: string | null;
	readonly timeoutMs: number;
}

export interface DesktopHTTPTransportResponse {
	readonly ok: boolean;
	readonly status: number;
	readonly statusText: string;
	readonly headers: Readonly<Record<string, string>>;
	readonly body: string | null;
}

export interface DesktopHTTPTransportAPI {
	fetch(request: DesktopHTTPTransportRequest): Promise<DesktopHTTPTransportResponse>;
}
