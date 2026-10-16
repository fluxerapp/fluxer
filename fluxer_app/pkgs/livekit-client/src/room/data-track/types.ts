// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Encryption_Type, DataTrackInfo as ProtocolDataTrackInfo} from '@livekit/protocol';
import type {DataTrackHandle} from './handle.ts';

export type DataTrackSid = string;

export type DataTrackInfo = {
	sid: DataTrackSid;
	pubHandle: DataTrackHandle;
	name: string;
	usesE2ee: boolean;
};

export type RemoteDataTrackPipelineOptions = {
	maxPartialFrames?: number;
};

export const DataTrackInfo = {
	from(protocolInfo: ProtocolDataTrackInfo): DataTrackInfo {
		return {
			sid: protocolInfo.sid,
			pubHandle: protocolInfo.pubHandle,
			name: protocolInfo.name,
			usesE2ee: protocolInfo.encryption !== Encryption_Type.NONE,
		};
	},
	toProtobuf(info: DataTrackInfo): ProtocolDataTrackInfo {
		return new ProtocolDataTrackInfo({
			sid: info.sid,
			pubHandle: info.pubHandle,
			name: info.name,
			encryption: info.usesE2ee ? Encryption_Type.GCM : Encryption_Type.NONE,
		});
	},
};
