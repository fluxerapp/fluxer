// SPDX-License-Identifier: AGPL-3.0-or-later

import {stripApiPrefix} from '@app/api/utils/RequestPathUtils';

export const OPERATOR_SOCKET_PATH = '/run/fluxer/operator.sock';
export const OPERATOR_HOST_USER_HEADER = 'X-Fluxer-Operator-Host-User';
export const OPERATOR_PEER_ADDRESS = '127.0.0.1';

const HOST_USER_MAX_LENGTH = 64;
const HOST_USER_PATTERN = /^[\p{L}\p{N}._@\\-]+$/u;

const operatorRequests = new WeakSet<Request>();

export function markOperatorRequest(request: Request): void {
	operatorRequests.add(request);
}

export function isOperatorRequest(request: Request): boolean {
	return operatorRequests.has(request);
}

export function isOperatorPath(path: string): boolean {
	const stripped = stripApiPrefix(path);
	return stripped === '/admin' || stripped.startsWith('/admin/');
}

export function readOperatorHostUser(request: Request): string | null {
	const value = request.headers.get(OPERATOR_HOST_USER_HEADER)?.trim();
	if (!value || value.length > HOST_USER_MAX_LENGTH || !HOST_USER_PATTERN.test(value)) {
		return null;
	}
	return value;
}
