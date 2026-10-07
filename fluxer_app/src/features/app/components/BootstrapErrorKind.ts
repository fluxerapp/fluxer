// SPDX-License-Identifier: AGPL-3.0-or-later

import {DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME} from '@fluxer/desktop_ipc/src/LocalAppRuntimeContract';
import {InstanceDiscoveryUnreachableError} from '@fluxer/instance_bootstrap/src/Discovery';

export const BootstrapErrorKind = Object.freeze({
	UNREACHABLE: 'unreachable',
	FAILED: 'failed',
} as const);

export type BootstrapErrorKind = (typeof BootstrapErrorKind)[keyof typeof BootstrapErrorKind];

const MAX_CAUSE_DEPTH = 8;

function reportsUnreachableInstance(error: unknown, depth: number): boolean {
	if (!(error instanceof Error) || depth > MAX_CAUSE_DEPTH) {
		return false;
	}
	if (
		error instanceof InstanceDiscoveryUnreachableError ||
		error.message.includes(DESKTOP_RUNTIME_DISCOVERY_UNREACHABLE_ERROR_NAME)
	) {
		return true;
	}
	if (error instanceof AggregateError && error.errors.some((inner) => reportsUnreachableInstance(inner, depth + 1))) {
		return true;
	}
	return reportsUnreachableInstance(error.cause, depth + 1);
}

export function classifyBootstrapError(error: unknown, online: boolean): BootstrapErrorKind {
	return !online || reportsUnreachableInstance(error, 0) ? BootstrapErrorKind.UNREACHABLE : BootstrapErrorKind.FAILED;
}
