// SPDX-License-Identifier: AGPL-3.0-or-later

import type {FederationPeerConfig} from '@app/api/federation/FederationConfig';
import type {SignedFederationEvent} from '@app/api/federation/FederationEvent';

export interface FederationDeliveryResult {
	status: number;
}

export interface IFederationTransport {
	send(peer: FederationPeerConfig, event: SignedFederationEvent): Promise<FederationDeliveryResult>;
}

export interface HttpFederationTransportOptions {
	timeoutMs: number;
	keyId: string;
}

export class HttpFederationTransport implements IFederationTransport {
	constructor(private readonly options: HttpFederationTransportOptions) {}

	async send(peer: FederationPeerConfig, event: SignedFederationEvent): Promise<FederationDeliveryResult> {
		const response = await fetch(`${peer.baseUrl}/federation/v1/events`, {
			method: 'POST',
			headers: {
				'content-type': 'application/json',
				'x-fluxer-key-id': this.options.keyId,
				'x-fluxer-origin': event.origin,
			},
			body: JSON.stringify(event),
			signal: AbortSignal.timeout(this.options.timeoutMs),
		});
		return {status: response.status};
	}
}
