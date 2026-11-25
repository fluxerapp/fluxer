// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	InstanceDiscoveryUnreachableError,
	InstanceRequiresNewerClientError,
} from '@fluxer/instance_bootstrap/src/Discovery';
import {selfSignedRootCertificateURL} from '@fluxer/instance_bootstrap/src/NetworkOrigin';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

export const CONNECTING_TO_INSTANCE_DESCRIPTOR = msg({
	message: 'Connecting to {domain}\u2026',
	comment:
		'Auth status shown while connecting to an instance, when the instance selector resolves it or when sign-in waits for the live connection. Domain is interpolated.',
});
export const INSTANCE_ADDRESS_INVALID_DESCRIPTOR = msg({
	message:
		"That doesn't look like an instance address. Try a domain such as {exampleInstanceDomain} or an IP address such as 192.168.1.20.",
	comment:
		'Instance selector error shown when the typed address cannot be an address at all. Example instance domain is interpolated. Keep the IP address as written.',
});
const INSTANCE_UNREACHABLE_DESCRIPTOR = msg({
	message: "Couldn't connect to a Fluxer instance at {domain}. Check the address and that the instance is online.",
	comment: 'Instance selector error shown when no Fluxer instance answered at the address. Domain is interpolated.',
});
const INSTANCE_UNREACHABLE_SELF_SIGNED_DESCRIPTOR = msg({
	message:
		"Couldn't connect to a Fluxer instance at {domain}. Check the address and that the instance is online. If the instance signs its own certificate, download it from {certificateUrl}, check that its SHA-256 fingerprint matches the one the instance's admin gives you, install it on this device, then try again.",
	comment:
		'Instance selector error shown when no Fluxer instance answered at an IP address or a name that only works on a local network, where the instance often uses a certificate it signed itself. The certificate comes over plain HTTP, so the fingerprint check guards against a swapped file. Domain and certificate URL are interpolated.',
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
		const certificateUrl = selfSignedRootCertificateURL(domain);
		if (certificateUrl != null) {
			return i18n._(INSTANCE_UNREACHABLE_SELF_SIGNED_DESCRIPTOR, {domain, certificateUrl});
		}
		return i18n._(INSTANCE_UNREACHABLE_DESCRIPTOR, {domain});
	}
	return i18n._(INSTANCE_CONNECT_FAILED_DESCRIPTOR, {domain});
}
