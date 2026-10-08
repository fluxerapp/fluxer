// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	InstanceDiscoveryUnreachableError,
	InstanceRequiresNewerClientError,
} from '@fluxer/instance_bootstrap/src/Discovery';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const CONNECTING_TO_INSTANCE_DESCRIPTOR = msg({
	message: 'Connecting to {domain}\u2026',
	comment:
		'Auth status shown while connecting to an instance, when the instance selector resolves it or when sign-in waits for the live connection. Domain is interpolated.',
});
export const INSTANCE_ADDRESS_INVALID_DESCRIPTOR = msg({
	message: "That doesn't look like an instance address. Try a domain such as {exampleInstanceDomain}.",
	comment:
		'Instance selector error shown when the typed address cannot be an address at all. Example instance domain is interpolated.',
});
const INSTANCE_UNREACHABLE_DESCRIPTOR = msg({
	message: "Couldn't reach a Fluxer instance at {domain}. Check the address and that the instance is online.",
	comment: 'Instance selector error shown when no Fluxer instance answered at the address. Domain is interpolated.',
});
const INSTANCE_NEEDS_NEWER_CLIENT_DESCRIPTOR = msg({
	message: '{domain} runs a newer version of Fluxer than this app. Update Fluxer, then try again.',
	comment:
		'Instance selector error shown when the instance speaks a protocol generation this client does not know. Domain is interpolated.',
});
export const INSTANCE_CONNECT_FAILED_DESCRIPTOR = msg({
	message: "Couldn't connect to {domain}. Try again.",
	comment:
		'Auth error shown when connecting to an instance fails, in the instance selector or when sign-in cannot reach the live connection. Domain is interpolated.',
});

export interface InstanceDiscoveryFailureRequest {
	readonly error: unknown;
	readonly domain: string;
	readonly i18n: I18n;
}

export function describeInstanceDiscoveryFailure({error, domain, i18n}: InstanceDiscoveryFailureRequest): string {
	if (error instanceof InstanceRequiresNewerClientError) {
		return i18n._(INSTANCE_NEEDS_NEWER_CLIENT_DESCRIPTOR, {domain});
	}
	if (error instanceof InstanceDiscoveryUnreachableError) {
		return i18n._(INSTANCE_UNREACHABLE_DESCRIPTOR, {domain});
	}
	return i18n._(INSTANCE_CONNECT_FAILED_DESCRIPTOR, {domain});
}
