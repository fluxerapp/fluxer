// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createCurrentBehaviorTestAccountPolicyEvaluator} from '../../test/AccountPolicyTestEvaluator';
import {setInjectedAccountPolicyEvaluator} from '../AccountPolicyService';
import {
	type IpInfoPrescreenOptions,
	ipInfoPrescreenOptionsFromEnv,
	type LocalIpIntel,
	prescreenIpInfoLookup,
} from '../RegistrationIpPrescreen';

const CLEAN_LOCAL: LocalIpIntel = {countryIso: 'SE', asn: 64500, asnOrg: 'Example Broadband ISP'};

function options(overrides: Partial<IpInfoPrescreenOptions> = {}): IpInfoPrescreenOptions {
	return {enabled: true, allowedAsns: new Set([64500]), ...overrides};
}

function local(overrides: Partial<LocalIpIntel> = {}): LocalIpIntel {
	return {...CLEAN_LOCAL, ...overrides};
}

beforeEach(() => {
	setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
});

afterEach(() => {
	setInjectedAccountPolicyEvaluator(undefined);
	delete process.env.FLUXER_RISK_IPINFO_PRESCREEN_ENABLED;
	delete process.env.FLUXER_RISK_IPINFO_PRESCREEN_ALLOW_ASNS;
});

describe('prescreenIpInfoLookup', () => {
	it('skips the lookup only when every local signal is clean and allowlisted', () => {
		expect(prescreenIpInfoLookup(local(), options())).toBe('skip');
	});

	it('consults when the pre-screen is disabled', () => {
		expect(prescreenIpInfoLookup(local(), options({enabled: false}))).toBe('consult');
	});

	it('consults when the allowlist is empty', () => {
		expect(prescreenIpInfoLookup(local(), options({allowedAsns: new Set()}))).toBe('consult');
	});

	it('consults when the local city database has no country', () => {
		expect(prescreenIpInfoLookup(local({countryIso: null}), options())).toBe('consult');
	});

	it('consults when the local ASN database has no ASN', () => {
		expect(prescreenIpInfoLookup(local({asn: null}), options())).toBe('consult');
	});

	it('consults when the ASN is not on the allowlist', () => {
		expect(prescreenIpInfoLookup(local({asn: 64501}), options())).toBe('consult');
	});

	it('consults for a trusted commercial privacy provider even on the allowlist', () => {
		expect(prescreenIpInfoLookup(local({asnOrg: 'Example Privacy Relay LLC'}), options())).toBe('consult');
	});

	it('consults for an education organization even on the allowlist', () => {
		expect(prescreenIpInfoLookup(local({asnOrg: 'North Example Academy'}), options())).toBe('consult');
	});

	it('consults for a cellular organization even on the allowlist', () => {
		expect(prescreenIpInfoLookup(local({asnOrg: 'Example cell-net Wireless'}), options())).toBe('consult');
	});

	it('skips when the allowlisted ASN carries no organization name to veto', () => {
		expect(prescreenIpInfoLookup(local({asnOrg: null}), options())).toBe('skip');
	});
});

describe('ipInfoPrescreenOptionsFromEnv', () => {
	it('is disabled with an empty allowlist by default', () => {
		const parsed = ipInfoPrescreenOptionsFromEnv();
		expect(parsed.enabled).toBe(false);
		expect(parsed.allowedAsns.size).toBe(0);
	});

	it('treats only 1 and true as enabled', () => {
		process.env.FLUXER_RISK_IPINFO_PRESCREEN_ENABLED = '1';
		expect(ipInfoPrescreenOptionsFromEnv().enabled).toBe(true);
		process.env.FLUXER_RISK_IPINFO_PRESCREEN_ENABLED = 'TRUE';
		expect(ipInfoPrescreenOptionsFromEnv().enabled).toBe(true);
		process.env.FLUXER_RISK_IPINFO_PRESCREEN_ENABLED = 'yes';
		expect(ipInfoPrescreenOptionsFromEnv().enabled).toBe(false);
		process.env.FLUXER_RISK_IPINFO_PRESCREEN_ENABLED = '0';
		expect(ipInfoPrescreenOptionsFromEnv().enabled).toBe(false);
	});

	it('parses a comma separated allowlist and drops non numeric entries', () => {
		process.env.FLUXER_RISK_IPINFO_PRESCREEN_ALLOW_ASNS = ' 64500, 64501 ,,notanasn,64502x,-3,64503 ';
		expect([...ipInfoPrescreenOptionsFromEnv().allowedAsns]).toEqual([64500, 64501, 64503]);
	});

	it('parses an empty allowlist from an empty or whitespace value', () => {
		process.env.FLUXER_RISK_IPINFO_PRESCREEN_ALLOW_ASNS = '';
		expect(ipInfoPrescreenOptionsFromEnv().allowedAsns.size).toBe(0);
		process.env.FLUXER_RISK_IPINFO_PRESCREEN_ALLOW_ASNS = '  ,  ,';
		expect(ipInfoPrescreenOptionsFromEnv().allowedAsns.size).toBe(0);
	});

	it('yields a consult verdict for every input with the shipped defaults', () => {
		const shipped = ipInfoPrescreenOptionsFromEnv();
		expect(prescreenIpInfoLookup(local(), shipped)).toBe('consult');
		expect(prescreenIpInfoLookup(local({asnOrg: null}), shipped)).toBe('consult');
	});
});
