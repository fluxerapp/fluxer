// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {DataTrackInfo} from './types.ts';

function isObject(subject: unknown): subject is object {
	return subject !== null && typeof subject === 'object';
}

export const TrackSymbol: symbol = Symbol.for('lk.track');

export interface ITrack {
	readonly trackSymbol: typeof TrackSymbol;
}

function isTrack(subject: unknown): subject is ITrack {
	return isObject(subject) && 'trackSymbol' in subject && subject.trackSymbol === TrackSymbol;
}

export interface ILocalTrack extends ITrack {
	readonly isLocal: true;

	isPublished(): boolean;
}

// @ts-expect-error - Export this in the future when cutting over to new track interfaces more widely
function _isLocalTrack(subject: unknown): subject is ILocalTrack {
	return isTrack(subject) && 'isLocal' in subject && subject.isLocal === true;
}

export const RemoteTrackSymbol: symbol = Symbol.for('lk.remote-track');

export interface IRemoteTrack extends ITrack {
	readonly isLocal: false;
}

// @ts-expect-error - Export this in the future when cutting over to new track interfaces more widely
function _isRemoteTrack(subject: unknown): subject is IRemoteTrack {
	return isTrack(subject) && 'localitySymbol' in subject && subject.localitySymbol === RemoteTrackSymbol;
}

export const DataTrackSymbol: symbol = Symbol.for('lk.data-track');
export interface IDataTrack extends ITrack {
	readonly typeSymbol: typeof DataTrackSymbol;

	readonly info?: DataTrackInfo;
}

export function isDataTrack(subject: unknown): subject is IDataTrack {
	return isTrack(subject) && 'typeSymbol' in subject && subject.typeSymbol === DataTrackSymbol;
}
