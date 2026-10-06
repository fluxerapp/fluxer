// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceRegionRecord, VoiceRegionWithServers, VoiceServerRecord} from '@app/api/voice/VoiceModel';

export interface IVoiceRepository {
	listRegions(): Promise<Array<VoiceRegionRecord>>;
	listRegionsWithServers(): Promise<Array<VoiceRegionWithServers>>;
	getRegion(id: string): Promise<VoiceRegionRecord | null>;
	getRegionWithServers(id: string): Promise<VoiceRegionWithServers | null>;
	upsertRegion(region: VoiceRegionRecord): Promise<void>;
	deleteRegion(regionId: string): Promise<void>;
	createRegion(region: Omit<VoiceRegionRecord, 'createdAt' | 'updatedAt'>): Promise<VoiceRegionRecord>;
	listServersForRegion(regionId: string): Promise<Array<VoiceServerRecord>>;
	listServers(regionId: string): Promise<Array<VoiceServerRecord>>;
	getServer(regionId: string, serverId: string): Promise<VoiceServerRecord | null>;
	createServer(server: Omit<VoiceServerRecord, 'createdAt' | 'updatedAt'>): Promise<VoiceServerRecord>;
	upsertServer(server: VoiceServerRecord): Promise<void>;
	deleteServer(regionId: string, serverId: string): Promise<void>;
}
