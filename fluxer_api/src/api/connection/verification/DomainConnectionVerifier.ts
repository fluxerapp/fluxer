// SPDX-License-Identifier: AGPL-3.0-or-later

import {Resolver} from 'node:dns/promises';
import {isFqdnHostname} from '@fluxer/schema/src/primitives/UrlValidators';
import {Logger} from '../../Logger';
import {EXTERNAL_RESPONSE_LIMITS} from '../../utils/ExternalResponseLimits';
import * as FetchUtils from '../../utils/FetchUtils';
import type {ConnectionVerificationParams, IConnectionVerifier} from './IConnectionVerifier';

const VERIFICATION_TIMEOUT_MS = 5000;
const DNS_VERIFICATION_TIMEOUT_MS = 2000;
const DOMAIN_VERIFICATION_DNS_SERVERS = [
	'1.1.1.1',
	'1.0.0.1',
	'8.8.8.8',
	'8.8.4.4',
	'9.9.9.9',
	'149.112.112.112',
	'208.67.222.222',
	'208.67.220.220',
];

interface TxtDnsResolver {
	resolveTxt(domain: string): Promise<Array<Array<string>>>;
	setServers(servers: Array<string>): void;
}

interface DomainConnectionVerifierOptions {
	dnsServers?: Array<string>;
	resolverFactory?: () => TxtDnsResolver;
}

export class DomainConnectionVerifier implements IConnectionVerifier {
	private readonly dnsServers: Array<string>;
	private readonly resolverFactory: () => TxtDnsResolver;

	constructor(options: DomainConnectionVerifierOptions = {}) {
		this.dnsServers = options.dnsServers ?? DOMAIN_VERIFICATION_DNS_SERVERS;
		this.resolverFactory =
			options.resolverFactory ??
			(() => new Resolver({timeout: DNS_VERIFICATION_TIMEOUT_MS, tries: 1, maxTimeout: DNS_VERIFICATION_TIMEOUT_MS}));
	}

	async verify(params: ConnectionVerificationParams): Promise<boolean> {
		const domain = params.identifier;
		const token = params.verification_token;
		const dnsResult = await this.checkDnsTxt(domain, token);
		if (dnsResult) {
			return true;
		}
		return this.checkWellKnown(domain, token);
	}

	private async checkDnsTxt(domain: string, token: string): Promise<boolean> {
		if (!isFqdnHostname(domain)) {
			return false;
		}
		const recordDomain = `_fluxer.${domain}`;
		const results = await Promise.allSettled(
			this.dnsServers.map(async (dnsServer) => {
				const resolver = this.resolverFactory();
				resolver.setServers([dnsServer]);
				return resolver.resolveTxt(recordDomain);
			}),
		);
		for (const result of results) {
			if (result.status === 'fulfilled' && this.recordsContainToken(result.value, token)) {
				return true;
			}
			if (result.status === 'rejected') {
				Logger.debug({domain, error: result.reason}, 'DNS TXT verification lookup failed');
			}
		}
		return false;
	}

	private recordsContainToken(records: Array<Array<string>>, token: string): boolean {
		for (const record of records) {
			const value = record.join('');
			if (value === `fluxer-verification=${token}`) {
				return true;
			}
		}
		return false;
	}

	private async checkWellKnown(domain: string, token: string): Promise<boolean> {
		const verificationPath = '/.well-known/fluxer-verification';
		let url: URL;
		try {
			url = new URL(verificationPath, `https://${domain}`);
		} catch {
			return false;
		}
		if (
			url.host !== domain ||
			url.username !== '' ||
			url.password !== '' ||
			url.port !== '' ||
			url.pathname !== verificationPath ||
			url.search !== '' ||
			url.hash !== ''
		) {
			return false;
		}
		try {
			const response = await FetchUtils.sendRequest({
				url: url.href,
				method: 'GET',
				timeout: VERIFICATION_TIMEOUT_MS,
				serviceName: 'connection_verification',
			});
			if (response.status < 200 || response.status >= 300) {
				return false;
			}
			const body = await FetchUtils.streamToStringWithLimit(response.stream, {
				maxBytes: EXTERNAL_RESPONSE_LIMITS.domainVerificationBytes,
				headers: response.headers,
				url: url.href,
				description: 'Domain verification response',
			});
			return body.trim() === token;
		} catch (error) {
			Logger.debug({domain, error}, 'Well-known verification lookup failed');
			return false;
		}
	}
}
