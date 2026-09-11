// SPDX-License-Identifier: AGPL-3.0-or-later

import {calculateDistance, parseCoordinate} from '../utils/GeoUtils';
import type {VoiceRegionAvailability, VoiceServerRecord} from './VoiceModel';

interface VoiceRegionPreference {
	regionId: string | null;
	mode: 'explicit' | 'automatic';
}

const DISTANCE_TIE_EPSILON_KM = 0.000001;

export function resolveVoiceRegionPreference({
	preferredRegionId,
	accessibleRegions,
	availableRegions,
	defaultRegionId,
}: {
	preferredRegionId: string | null;
	accessibleRegions: Array<VoiceRegionAvailability>;
	availableRegions: Array<VoiceRegionAvailability>;
	defaultRegionId: string | null;
}): VoiceRegionPreference {
	const accessibleRegionIds = new Set(accessibleRegions.map((region) => region.id));
	if (preferredRegionId) {
		if (accessibleRegionIds.has(preferredRegionId)) {
			return {regionId: preferredRegionId, mode: 'explicit'};
		}
		return {regionId: null, mode: 'automatic'};
	}
	if (defaultRegionId && accessibleRegionIds.has(defaultRegionId)) {
		return {regionId: defaultRegionId, mode: 'automatic'};
	}
	const defaultRegion =
		accessibleRegions.find((region) => region.isDefault) ?? availableRegions.find((region) => region.isDefault) ?? null;
	if (defaultRegion) {
		return {regionId: defaultRegion.id, mode: 'automatic'};
	}
	const fallbackRegion = accessibleRegions[0] ?? availableRegions[0] ?? null;
	return {regionId: fallbackRegion ? fallbackRegion.id : null, mode: 'automatic'};
}

export function selectVoiceRegionId({
	preferredRegionId,
	mode,
	accessibleRegions,
	availableRegions,
	latitude,
	longitude,
	selectionKey,
}: {
	preferredRegionId: string | null;
	mode: VoiceRegionPreference['mode'];
	accessibleRegions: Array<VoiceRegionAvailability>;
	availableRegions: Array<VoiceRegionAvailability>;
	latitude?: string;
	longitude?: string;
	selectionKey: string;
}): string | null {
	if (mode === 'automatic' && accessibleRegions.length > 0) {
		const closestRegionIds = findClosestRegionIds(latitude, longitude, accessibleRegions);
		const closestRegionId = selectBalancedRegionId(closestRegionIds, selectionKey);
		if (closestRegionId !== null) {
			return closestRegionId;
		}
	}
	if (preferredRegionId) {
		return preferredRegionId;
	}
	const accessibleFallback = accessibleRegions[0];
	if (accessibleFallback) {
		return accessibleFallback.id;
	}
	return availableRegions[0]?.id ?? null;
}

export function selectClosestPseudoRegionServer({
	mode,
	accessibleServers,
	connectionCounts,
	latitude,
	longitude,
	selectionKey,
}: {
	mode: VoiceRegionPreference['mode'];
	accessibleServers: Array<VoiceServerRecord>;
	connectionCounts: ReadonlyMap<string, number>;
	latitude?: string;
	longitude?: string;
	selectionKey: string;
}): VoiceServerRecord | null {
	if (mode !== 'automatic') {
		return null;
	}
	const userLat = parseCoordinate(latitude);
	const userLon = parseCoordinate(longitude);
	if (userLat === null || userLon === null) {
		return null;
	}
	const preferredServers = preferServersUnderSoftLimit(accessibleServers, connectionCounts);
	const closestServers = findClosestServers(preferredServers, userLat, userLon);
	return selectBalancedServer(closestServers, selectionKey);
}

export function preferServersUnderSoftLimit(
	servers: Array<VoiceServerRecord>,
	connectionCounts: ReadonlyMap<string, number>,
): Array<VoiceServerRecord> {
	const serversUnderLimit = servers.filter((server) => !isServerAtSoftLimit(server, connectionCounts));
	return serversUnderLimit.length > 0 ? serversUnderLimit : servers;
}

function isServerAtSoftLimit(server: VoiceServerRecord, connectionCounts: ReadonlyMap<string, number>): boolean {
	const limit = server.softConnectionLimit;
	if (limit === null || limit <= 0) {
		return false;
	}
	const connectionCount = connectionCounts.get(server.serverId);
	if (connectionCount === undefined) {
		return false;
	}
	return connectionCount >= limit;
}

function findClosestRegionIds(
	latitude: string | undefined,
	longitude: string | undefined,
	accessibleRegions: Array<VoiceRegionAvailability>,
): Array<string> {
	const userLat = parseCoordinate(latitude);
	const userLon = parseCoordinate(longitude);
	if (userLat === null || userLon === null) {
		return [];
	}
	const closestRegionIds: Array<string> = [];
	let minDistance = Number.POSITIVE_INFINITY;
	for (const region of accessibleRegions) {
		const distance = calculateDistance(userLat, userLon, region.latitude, region.longitude);
		if (distance + DISTANCE_TIE_EPSILON_KM < minDistance) {
			minDistance = distance;
			closestRegionIds.length = 0;
			closestRegionIds.push(region.id);
			continue;
		}
		if (Math.abs(distance - minDistance) <= DISTANCE_TIE_EPSILON_KM) {
			closestRegionIds.push(region.id);
		}
	}
	return closestRegionIds;
}

function findClosestServers(
	accessibleServers: Array<VoiceServerRecord>,
	userLat: number,
	userLon: number,
): Array<VoiceServerRecord> {
	const closestServers: Array<VoiceServerRecord> = [];
	let minDistance = Number.POSITIVE_INFINITY;
	for (const server of accessibleServers) {
		if (server.latitude === null || server.longitude === null) {
			continue;
		}
		const distance = calculateDistance(userLat, userLon, server.latitude, server.longitude);
		if (distance + DISTANCE_TIE_EPSILON_KM < minDistance) {
			minDistance = distance;
			closestServers.length = 0;
			closestServers.push(server);
			continue;
		}
		if (Math.abs(distance - minDistance) <= DISTANCE_TIE_EPSILON_KM) {
			closestServers.push(server);
		}
	}
	return closestServers;
}

function selectBalancedRegionId(regionIds: Array<string>, selectionKey: string): string | null {
	if (regionIds.length === 0) {
		return null;
	}
	const sortedRegionIds = [...regionIds].sort(compareText);
	return sortedRegionIds[stableSelectionIndex(sortedRegionIds.length, selectionKey)] ?? null;
}

function selectBalancedServer(servers: Array<VoiceServerRecord>, selectionKey: string): VoiceServerRecord | null {
	if (servers.length === 0) {
		return null;
	}
	const sortedServers = [...servers].sort(compareVoiceServers);
	return sortedServers[stableSelectionIndex(sortedServers.length, selectionKey)] ?? null;
}

function stableSelectionIndex(length: number, selectionKey: string): number {
	return hashSelectionKey(selectionKey) % length;
}

function hashSelectionKey(selectionKey: string): number {
	let hash = 0x811c9dc5;
	for (let index = 0; index < selectionKey.length; index += 1) {
		hash ^= selectionKey.charCodeAt(index);
		hash = Math.imul(hash, 0x01000193);
	}
	return hash >>> 0;
}

function compareText(left: string, right: string): number {
	if (left < right) {
		return -1;
	}
	if (left > right) {
		return 1;
	}
	return 0;
}

function compareVoiceServers(left: VoiceServerRecord, right: VoiceServerRecord): number {
	const regionComparison = compareText(left.regionId, right.regionId);
	if (regionComparison !== 0) {
		return regionComparison;
	}
	return compareText(left.serverId, right.serverId);
}
