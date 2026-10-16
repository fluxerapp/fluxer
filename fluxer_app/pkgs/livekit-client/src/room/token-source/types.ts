// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {JWTPayload} from 'jose';
import type {ValueToSnakeCase} from '../../utils/camelToSnakeCase.ts';

export type RoomAgentDispatchObject = {
	agentName: string;
	metadata: string;
};

export type RoomConfigurationObject = {
	name?: string;
	emptyTimeout?: number;
	departureTimeout?: number;
	maxParticipants?: number;
	metadata?: string;
	egress?: unknown;
	minPlayoutDelay?: number;
	maxPlayoutDelay?: number;
	syncStreams?: boolean;
	agents?: Array<RoomAgentDispatchObject>;
};

export type TokenSourceRequestObject = {
	roomName?: string;
	participantName?: string;
	participantIdentity?: string;
	participantMetadata?: string;
	participantAttributes?: Record<string, string>;
	roomConfig?: RoomConfigurationObject;
};

export type TokenSourceResponseObject = {
	serverUrl: string;
	participantToken: string;
};

export type TokenSourceRequestPayload = ValueToSnakeCase<TokenSourceRequestObject>;

export type TokenSourceResponsePayload = ValueToSnakeCase<TokenSourceResponseObject>;

export type TokenPayload = JWTPayload & {
	name?: string;
	metadata?: string;
	attributes?: Record<string, string>;
	video?: {
		room?: string;
		roomJoin?: boolean;
		canPublish?: boolean;
		canPublishData?: boolean;
		canSubscribe?: boolean;
	};
	roomConfig?: RoomConfigurationObject;
};

export abstract class TokenSourceFixed {
	abstract fetch(): Promise<TokenSourceResponseObject>;
}

export type TokenSourceFetchOptions = {
	roomName?: string;
	participantName?: string;
	participantIdentity?: string;
	participantMetadata?: string;
	participantAttributes?: {[key: string]: string};

	agentName?: string;
	agentMetadata?: string;
	deployment?: string;
};

export abstract class TokenSourceConfigurable {
	abstract fetch(options: TokenSourceFetchOptions): Promise<TokenSourceResponseObject>;
	abstract fetch(options: TokenSourceFetchOptions, force?: boolean): Promise<TokenSourceResponseObject>;
}

export type TokenSourceBase = TokenSourceFixed | TokenSourceConfigurable;
