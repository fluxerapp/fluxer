// SPDX-License-Identifier: AGPL-3.0-or-later

import {matchTorHostname} from '@app/api/risk/adapters/TorReverseDnsChecker';
import {describe, expect, it} from 'vitest';

describe('matchTorHostname', () => {
	it.each([
		['tor-exit.example.com'],
		['exit1.tor.example.net'],
		['tor-exit-01.example.net'],
		['torexit23.example.net'],
		['tor-relay.example.com'],
		['anonymizer.example.com'],
		['anonymiser.example.com'],
		['anon-proxy.example.com'],
		['tor01.example.com'],
		['foo.torproject.example'],
	])('matches Tor-operator pattern: %s', (hostname) => {
		expect(matchTorHostname(hostname)).not.toBeNull();
	});
	it.each([
		['ec2-1-2-3-4.us-east-1.compute.amazonaws.com'],
		['ip-10-0-0-1.ec2.internal'],
		['example.com'],
		['mx01.google.com'],
		['a1234.hetzner.example'],
		['contoro.example.com'],
	])('does not match benign hostname: %s', (hostname) => {
		expect(matchTorHostname(hostname)).toBeNull();
	});
});
