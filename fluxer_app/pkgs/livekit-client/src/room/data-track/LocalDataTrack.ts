// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import log, {getLogger, LoggerNames, type StructuredLogger} from '../../logger.ts';
import {Future} from '../utils.ts';
import {type DataTrackFrame, DataTrackFrameInternal} from './frame.ts';
import type {DataTrackHandle} from './handle.ts';
import {DataTrackPushFrameError} from './outgoing/errors.ts';
import type OutgoingDataTrackManager from './outgoing/OutgoingDataTrackManager.ts';
import type {DataTrackOptions, EventPacketsFlushedChange} from './outgoing/types.ts';
import {DataTrackSymbol, type IDataTrack, type ILocalTrack, TrackSymbol} from './track-interfaces.ts';
import type {DataTrackInfo} from './types.ts';

export default class LocalDataTrack implements ILocalTrack, IDataTrack {
	readonly trackSymbol = TrackSymbol;

	readonly isLocal = true;

	readonly typeSymbol = DataTrackSymbol;

	protected options: DataTrackOptions;

	protected handle: DataTrackHandle | null = null;

	protected manager: OutgoingDataTrackManager;

	protected log: StructuredLogger = log;

	protected flushedFuture = new Future<void, never>();

	protected isFlushed = true;

	constructor(options: DataTrackOptions, manager: OutgoingDataTrackManager) {
		this.options = options;
		this.manager = manager;

		this.log = getLogger(LoggerNames.DataTracks);

		this.manager.on('packetsFlushedChange', this.handleManagerPacketsFlushedChange);
		this.manager.on('reset', this.handleManagerReset);
	}

	private handleManagerReset = () => {
		this.flushedFuture.resolve?.();

		this.manager.off('packetsFlushedChange', this.handleManagerPacketsFlushedChange);
		this.manager.off('reset', this.handleManagerReset);
	};

	private handleManagerPacketsFlushedChange = (event: EventPacketsFlushedChange) => {
		this.isFlushed = event.isFlushed;
		if (event.isFlushed) {
			this.flushedFuture.resolve?.();
			this.flushedFuture = new Future();
		}
	};

	static withExplicitHandle(options: DataTrackOptions, manager: OutgoingDataTrackManager, handle: DataTrackHandle) {
		const track = new LocalDataTrack(options, manager);
		track.handle = handle;
		return track;
	}

	get info() {
		const descriptor = this.descriptor;
		if (descriptor?.type === 'active') {
			return descriptor.info;
		} else {
			return undefined;
		}
	}

	protected get descriptor() {
		return this.handle ? this.manager.getDescriptor(this.handle) : null;
	}

	async publish(signal?: AbortSignal) {
		this.handle = await this.manager.publishRequest(this.options, signal);
	}

	isPublished(): this is {info: DataTrackInfo} {
		return this.descriptor?.type === 'active' && this.descriptor.publishState !== 'unpublished';
	}

	tryPush(frame: DataTrackFrame) {
		if (!this.handle) {
			throw DataTrackPushFrameError.trackUnpublished();
		}

		const internalFrame = DataTrackFrameInternal.from(frame);
		return this.manager.tryProcessAndSend(this.handle, internalFrame);
	}

	async flush(): Promise<void> {
		if (this.isFlushed) {
			return;
		}
		return this.flushedFuture.promise;
	}

	async unpublish() {
		if (!this.handle) {
			log.warn(`Data track "${this.options.name}" is not published, so unpublishing has no effect.`);
			return;
		}
		await this.manager.unpublishRequest(this.handle);
	}
}
