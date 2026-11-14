// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {version as v} from '../package.json';

export const version = v;
export const protocolVersion = 17;

export const CLIENT_PROTOCOL_DEFAULT = 0;
export const CLIENT_PROTOCOL_DATA_STREAM_RPC = 1;
export const CLIENT_PROTOCOL_DATA_STREAM_V2 = 2;

export const clientProtocol = CLIENT_PROTOCOL_DATA_STREAM_V2;
