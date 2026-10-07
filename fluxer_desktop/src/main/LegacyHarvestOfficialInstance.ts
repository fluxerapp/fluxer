// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StagedLegacyHarvest} from '@electron/main/LegacyOriginHarvestStore';
import {officialClientApiEndpointForAlias} from '@fluxer/instance_bootstrap/src/OfficialInstance';

const SAME_ORIGIN_API_PATH = '/api';
const ACCOUNT_STORAGE_KEY_SEPARATOR = '::';
const ACCOUNT_DATABASE = 'FluxerAccounts';

interface OfficialInstanceRekey {
	readonly harvestedOrigin: string;
	readonly harvestedApiEndpoint: string;
	readonly officialOrigin: string;
	readonly officialApiEndpoint: string;
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
	if (typeof value !== 'object' || value === null) {
		return false;
	}
	const prototype = Object.getPrototypeOf(value);
	return prototype === Object.prototype || prototype === null;
}

function planOfficialInstanceRekey(harvestedOrigin: string): OfficialInstanceRekey | null {
	const harvestedApiEndpoint = `${harvestedOrigin}${SAME_ORIGIN_API_PATH}`;
	const officialApiEndpoint = officialClientApiEndpointForAlias(harvestedApiEndpoint);
	if (officialApiEndpoint === null || officialApiEndpoint === harvestedApiEndpoint) {
		return null;
	}
	return {
		harvestedOrigin,
		harvestedApiEndpoint,
		officialOrigin: new URL(officialApiEndpoint).origin,
		officialApiEndpoint,
	};
}

function rekeyText(text: string, rekey: OfficialInstanceRekey): string {
	return text.replaceAll(
		`${rekey.harvestedApiEndpoint}${ACCOUNT_STORAGE_KEY_SEPARATOR}`,
		`${rekey.officialApiEndpoint}${ACCOUNT_STORAGE_KEY_SEPARATOR}`,
	);
}

function rekeyValue(value: unknown, rekey: OfficialInstanceRekey): unknown {
	if (typeof value === 'string') {
		return rekeyText(value, rekey);
	}
	if (Array.isArray(value)) {
		return value.map((item) => rekeyValue(item, rekey));
	}
	if (!isPlainObject(value)) {
		return value;
	}
	const rekeyed: Record<string, unknown> = {};
	for (const [key, item] of Object.entries(value)) {
		rekeyed[rekeyText(key, rekey)] = rekeyValue(item, rekey);
	}
	return rekeyed;
}

function rekeyAccountInstance(value: unknown, rekey: OfficialInstanceRekey): unknown {
	if (!isPlainObject(value) || !isPlainObject(value.instance)) {
		return value;
	}
	const instance = {...value.instance};
	if (instance.apiEndpoint === rekey.harvestedApiEndpoint) {
		instance.apiEndpoint = rekey.officialApiEndpoint;
	}
	if (instance.webAppEndpoint === rekey.harvestedOrigin) {
		instance.webAppEndpoint = rekey.officialOrigin;
	}
	return {...value, instance};
}

export function rekeyHarvestToOfficialInstance(harvest: StagedLegacyHarvest): StagedLegacyHarvest {
	const rekey = planOfficialInstanceRekey(harvest.origin);
	if (rekey === null) {
		return harvest;
	}
	const localStorage: Record<string, string> = {};
	for (const [name, value] of Object.entries(harvest.localStorage)) {
		localStorage[rekeyText(name, rekey)] = rekeyText(value, rekey);
	}
	return {
		...harvest,
		localStorage,
		stores: harvest.stores.map((store) => ({
			...store,
			records: store.records.map((record) => {
				const value = rekeyValue(record.value, rekey);
				return {
					key: rekeyValue(record.key, rekey),
					value: store.database === ACCOUNT_DATABASE ? rekeyAccountInstance(value, rekey) : value,
				};
			}),
		})),
	};
}
