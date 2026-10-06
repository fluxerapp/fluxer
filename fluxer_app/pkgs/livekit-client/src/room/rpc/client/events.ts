// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {DataPacket} from '@livekit/protocol';

export type EventSendDataPacket = {
	packet: DataPacket;
};

export type RpcClientManagerCallbacks = {
	sendDataPacket: (event: EventSendDataPacket) => void;
};
