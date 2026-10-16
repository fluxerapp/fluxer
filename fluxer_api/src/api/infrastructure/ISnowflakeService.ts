// SPDX-License-Identifier: AGPL-3.0-or-later

export interface ISnowflakeService {
	initialize(): Promise<void>;
	shutdown(): Promise<void>;
	generate(): Promise<bigint>;
	generateForChannel(channelId: string | bigint): Promise<bigint>;
}
