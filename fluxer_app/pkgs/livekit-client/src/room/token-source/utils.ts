// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {JsonObject} from '@bufbuild/protobuf';
import {RoomConfiguration, type TokenSourceResponse} from '@livekit/protocol';
import {decodeJwt} from 'jose';
import type {RoomConfigurationObject, TokenPayload, TokenSourceFetchOptions} from './types.ts';

const ONE_SECOND_IN_MILLISECONDS = 1000;
const ONE_MINUTE_IN_MILLISECONDS = 60 * ONE_SECOND_IN_MILLISECONDS;

export function isResponseTokenValid(response: TokenSourceResponse) {
	const jwtPayload = decodeTokenPayload(response.participantToken);
	if (!jwtPayload?.exp) {
		return false;
	}

	const now = new Date();

	if (jwtPayload.nbf) {
		const nbfInMilliseconds = jwtPayload.nbf * ONE_SECOND_IN_MILLISECONDS;
		const nbfDate = new Date(nbfInMilliseconds);
		if (nbfDate > now) {
			return false;
		}
	}

	const expInMilliseconds = jwtPayload.exp * ONE_SECOND_IN_MILLISECONDS;
	const expDate = new Date(expInMilliseconds - ONE_MINUTE_IN_MILLISECONDS);

	return expDate > now;
}

export function decodeTokenPayload(token: string) {
	const payload = decodeJwt<Omit<TokenPayload, 'roomConfig'>>(token);

	const {roomConfig, ...rest} = payload;

	const mappedPayload: TokenPayload = {
		...rest,
		roomConfig: payload.roomConfig
			? (RoomConfiguration.fromJson(toJsonObject(payload.roomConfig), {
					ignoreUnknownFields: true,
				}) as RoomConfigurationObject)
			: undefined,
	};

	return mappedPayload;
}

function toJsonObject(value: unknown): JsonObject {
	if (value && typeof value === 'object' && !Array.isArray(value)) {
		return value as JsonObject;
	}
	throw new TypeError('Expected roomConfig to be a JSON object');
}

export function areTokenSourceFetchOptionsEqual(a: TokenSourceFetchOptions, b: TokenSourceFetchOptions) {
	const allKeysSet = new Set([...Object.keys(a), ...Object.keys(b)]) as Set<keyof TokenSourceFetchOptions>;

	for (const key of allKeysSet) {
		switch (key) {
			case 'roomName':
			case 'participantName':
			case 'participantIdentity':
			case 'participantMetadata':
			case 'participantAttributes':
			case 'agentName':
			case 'agentMetadata':
			case 'deployment':
				if (a[key] !== b[key]) {
					return false;
				}
				break;
			default: {
				const exhaustiveCheckedKey: never = key;
				throw new Error(`Options key ${exhaustiveCheckedKey} not being checked for equality!`);
			}
		}
	}

	return true;
}
