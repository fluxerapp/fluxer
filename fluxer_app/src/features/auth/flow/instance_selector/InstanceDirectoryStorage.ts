// SPDX-License-Identifier: AGPL-3.0-or-later

import {IS_DEV} from '@app/features/platform/types/Env';
import type {
	DesktopKnownInstanceRecord,
	DesktopKnownInstanceStorageAPI,
} from '@fluxer/desktop_ipc/src/KnownInstanceContract';
import {InstanceEndpointKind, normalizeInstanceEndpoint} from '@fluxer/instance_bootstrap/src/EndpointNormalization';
import {isOfficialInstanceHost, OFFICIAL_INSTANCE_NAME} from '@fluxer/instance_bootstrap/src/OfficialInstance';

const MAX_KNOWN_INSTANCE_ROWS = 5;

const HTTPS_DISPLAY_PREFIX = 'https://';

const LOCAL_DEVELOPMENT_INSTANCE_KEY = 'local-development';

export interface InstanceInfo {
	instanceKey: string;
	domain: string;
	name: string | null;
	lastUsed: number;
}

export function normalizeInstanceDomain(domain: string): string | null {
	const endpoint = normalizeInstanceEndpoint(domain, InstanceEndpointKind.SERVICE);
	if (endpoint == null) {
		return null;
	}
	if (endpoint.startsWith(HTTPS_DISPLAY_PREFIX)) {
		return endpoint.slice(HTTPS_DISPLAY_PREFIX.length);
	}
	return endpoint;
}

export function normalizeInstanceName(name: string | null | undefined): string | null {
	if (name == null) {
		return null;
	}
	const trimmed = name.trim();
	if (trimmed.length === 0) {
		return null;
	}
	return trimmed;
}

export function instanceDomainHost(domain: string): string {
	const origin = instanceDomainOrigin(domain);
	if (origin == null) {
		return domain;
	}
	return new URL(origin).host;
}

export function resolveInstanceLabel(name: string | null | undefined, domain: string): string {
	const normalizedName = normalizeInstanceName(name);
	if (isOfficialInstanceHost(domain)) {
		return normalizedName ?? OFFICIAL_INSTANCE_NAME;
	}
	if (
		normalizedName == null ||
		normalizedName === domain ||
		normalizedName.toLowerCase() === OFFICIAL_INSTANCE_NAME.toLowerCase()
	) {
		return instanceDomainHost(domain);
	}
	return normalizedName;
}

export function isOfficialInstanceInfo(instance: InstanceInfo): boolean {
	return isOfficialInstanceHost(instance.instanceKey) || isOfficialInstanceHost(instance.domain);
}

export function shouldSaveKnownInstance(domain: string): boolean {
	return !isOfficialInstanceHost(domain);
}

function instanceDomainOrigin(domain: string): string | null {
	try {
		return new URL(domain.includes('://') ? domain : `${HTTPS_DISPLAY_PREFIX}${domain}`).origin;
	} catch {
		return null;
	}
}

export function resolveLocalDevelopmentInstance(knownInstances: ReadonlyArray<InstanceInfo>): InstanceInfo | null {
	const url = globalThis.window?.electron?.localDevelopmentInstanceUrl;
	if (url == null) {
		return null;
	}
	const domain = normalizeInstanceDomain(url);
	const origin = instanceDomainOrigin(url);
	if (domain == null || origin == null) {
		return null;
	}
	if (knownInstances.some((instance) => instanceDomainOrigin(instance.domain) === origin)) {
		return null;
	}
	return {instanceKey: LOCAL_DEVELOPMENT_INSTANCE_KEY, domain, name: new URL(origin).host, lastUsed: 0};
}

function getDesktopKnownInstanceAPI(): DesktopKnownInstanceStorageAPI | null {
	return globalThis.window?.electron?.desktopKnownInstances ?? null;
}

function requireDesktopKnownInstanceAPI(): DesktopKnownInstanceStorageAPI | null {
	if (IS_DEV && globalThis.window?.electron == null) {
		throw new Error('The known-instance directory is desktop-only and was written to without Electron');
	}
	return getDesktopKnownInstanceAPI();
}

function toInstanceInfo(record: DesktopKnownInstanceRecord): InstanceInfo {
	return {
		instanceKey: record.instanceKey,
		domain: record.domain,
		name: normalizeInstanceName(record.displayName),
		lastUsed: record.lastUsed,
	};
}

export async function loadKnownInstances(): Promise<Array<InstanceInfo>> {
	const knownInstanceAPI = getDesktopKnownInstanceAPI();
	if (knownInstanceAPI == null) {
		return [];
	}
	const records = await knownInstanceAPI.getAll();
	return records
		.map(toInstanceInfo)
		.sort((left, right) => right.lastUsed - left.lastUsed)
		.slice(0, MAX_KNOWN_INSTANCE_ROWS);
}

export async function saveKnownInstance(input: DesktopKnownInstanceRecord): Promise<InstanceInfo> {
	const domain = normalizeInstanceDomain(input.domain) ?? input.domain.trim();
	const record: DesktopKnownInstanceRecord = {
		instanceKey: input.instanceKey,
		domain,
		displayName: normalizeInstanceName(input.displayName) ?? domain,
		lastUsed: input.lastUsed,
	};
	const knownInstanceAPI = requireDesktopKnownInstanceAPI();
	if (knownInstanceAPI != null) {
		await knownInstanceAPI.upsert(record);
	}
	return toInstanceInfo(record);
}

export async function deleteKnownInstance(instanceKey: string): Promise<void> {
	const knownInstanceAPI = requireDesktopKnownInstanceAPI();
	if (knownInstanceAPI == null) {
		return;
	}
	await knownInstanceAPI.delete(instanceKey);
}
