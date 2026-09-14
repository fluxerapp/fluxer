// SPDX-License-Identifier: AGPL-3.0-or-later

import {isIPv4} from 'node:net';
import {getIpNetworkKey} from '@fluxer/ip_utils/src/IpAddress';

interface ValidateOutboundEndpointOptions {
	name: string;
	allowHttp: boolean;
	allowLocalhost: boolean;
	allowPrivateIpLiterals: boolean;
}

export function validateOutboundEndpointUrl(rawEndpoint: string, options: ValidateOutboundEndpointOptions): URL {
	let endpointUrl: URL;
	try {
		endpointUrl = new URL(rawEndpoint);
	} catch {
		throw new Error(`${options.name} must be a valid URL`);
	}
	if (endpointUrl.protocol !== 'http:' && endpointUrl.protocol !== 'https:') {
		throw new Error(`${options.name} must use http or https`);
	}
	if (endpointUrl.protocol === 'http:' && !options.allowHttp) {
		throw new Error(`${options.name} must use https`);
	}
	if (endpointUrl.username || endpointUrl.password) {
		throw new Error(`${options.name} must not include URL credentials`);
	}
	if (endpointUrl.search || endpointUrl.hash) {
		throw new Error(`${options.name} must not include query string or fragment`);
	}
	const hostname = endpointUrl.hostname.toLowerCase();
	if (!hostname) {
		throw new Error(`${options.name} must include a hostname`);
	}
	if (!options.allowLocalhost && isLocalhostHostname(hostname)) {
		throw new Error(`${options.name} cannot use localhost`);
	}
	if (!options.allowPrivateIpLiterals && isPrivateOrSpecialIpLiteral(hostname)) {
		throw new Error(`${options.name} cannot use private or special IP literals`);
	}
	return endpointUrl;
}

function normalizeEndpointOrigin(endpointUrl: URL): string {
	const pathname = endpointUrl.pathname.endsWith('/') ? endpointUrl.pathname.slice(0, -1) : endpointUrl.pathname;
	const normalisedPath = pathname === '/' ? '' : pathname;
	return `${endpointUrl.protocol}//${endpointUrl.host}${normalisedPath}`;
}

export function buildEndpointUrl(endpointUrl: URL, path: string): string {
	const trimmedPath = path.trim();
	if (!trimmedPath) {
		return normalizeEndpointOrigin(endpointUrl);
	}
	const lowercasePath = trimmedPath.toLowerCase();
	if (lowercasePath.startsWith('http://') || lowercasePath.startsWith('https://') || trimmedPath.startsWith('//')) {
		throw new Error('Outbound path must be relative');
	}
	const prefixedPath = trimmedPath.startsWith('/') ? trimmedPath : `/${trimmedPath}`;
	return `${normalizeEndpointOrigin(endpointUrl)}${prefixedPath}`;
}

function isLocalhostHostname(hostname: string): boolean {
	return hostname === 'localhost' || hostname.endsWith('.localhost');
}

function isPrivateOrSpecialIpLiteral(hostname: string): boolean {
	const address = getIpNetworkKey(hostname, {ipv4PrefixLength: 'exact', ipv6PrefixLength: 'exact'});
	if (address === null) {
		return false;
	}
	if (isIPv4(address)) {
		return isPrivateOrSpecialIPv4(address);
	}
	return isPrivateOrSpecialIPv6(address);
}

function isPrivateOrSpecialIPv4(hostname: string): boolean {
	const octets = hostname.split('.').map((part) => Number(part));
	if (octets.length !== 4 || octets.some((octet) => !Number.isInteger(octet) || octet < 0 || octet > 255)) {
		return true;
	}
	const [first, second] = octets;
	if (first === 0 || first === 10 || first === 127) return true;
	if (first === 169 && second === 254) return true;
	if (first === 172 && second >= 16 && second <= 31) return true;
	if (first === 192 && second === 168) return true;
	if (first === 100 && second >= 64 && second <= 127) return true;
	if (first === 198 && (second === 18 || second === 19)) return true;
	if (first >= 224) return true;
	return false;
}

function isPrivateOrSpecialIPv6(address: string): boolean {
	if (address === '::' || address === '::1') {
		return true;
	}
	const firstGroup = Number.parseInt(address.split(':', 1)[0] || '0', 16);
	return (firstGroup & 0xffc0) === 0xfe80 || (firstGroup & 0xfe00) === 0xfc00 || (firstGroup & 0xff00) === 0xff00;
}
