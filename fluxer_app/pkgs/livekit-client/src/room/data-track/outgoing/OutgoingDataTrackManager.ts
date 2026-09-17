// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import {EventEmitter} from 'events';
import type TypedEmitter from 'typed-emitter';
import type {BaseE2EEManager} from '../../../e2ee/E2eeManager.ts';
import {getLogger, LoggerNames} from '../../../logger.ts';
import {abortSignalAny, abortSignalTimeout} from '../../../utils/abort-signal-polyfill.ts';
import type {Throws} from '../../../utils/throws.ts';
import {Future} from '../../utils.ts';
import type {DataTrackFrameInternal} from '../frame.ts';
import {type DataTrackHandle, DataTrackHandleAllocator} from '../handle.ts';
import LocalDataTrack from '../LocalDataTrack.ts';
import type {DataTrackInfo} from '../types.ts';
import {DataTrackPublishError, DataTrackPushFrameError, type DataTrackPushFrameErrorReason} from './errors.ts';
import DataTrackOutgoingPipeline from './pipeline.ts';
import type {
	DataTrackOptions,
	EventPacketAvailable,
	EventPacketsFlushedChange,
	EventSfuPublishRequest,
	EventSfuUnpublishRequest,
	EventTrackPublished,
	EventTrackUnpublished,
	SfuPublishResponseResult,
} from './types.ts';

const log = getLogger(LoggerNames.DataTracks);

export type PendingDescriptor = {
	type: 'pending';
	completionFuture: Future<void, DataTrackPublishError>;
};
export type ActiveDescriptor = {
	type: 'active';
	info: DataTrackInfo;

	publishState: 'published' | 'republishing' | 'unpublished';

	pipeline: DataTrackOutgoingPipeline;

	unpublishingFuture: Future<void, never>;
};
export type Descriptor = PendingDescriptor | ActiveDescriptor;

export const Descriptor = {
	pending(): PendingDescriptor {
		return {
			type: 'pending',
			completionFuture: new Future(),
		};
	},
	active(info: DataTrackInfo, e2eeManager: BaseE2EEManager | null): ActiveDescriptor {
		return {
			type: 'active',
			info,
			publishState: 'published',
			pipeline: new DataTrackOutgoingPipeline({info, e2eeManager}),
			unpublishingFuture: new Future(),
		};
	},
};

export type DataTrackOutgoingManagerCallbacks = {
	sfuPublishRequest: (event: EventSfuPublishRequest) => void;
	sfuUnpublishRequest: (event: EventSfuUnpublishRequest) => void;
	packetAvailable: (event: EventPacketAvailable) => void;
	trackPublished: (event: EventTrackPublished) => void;
	trackUnpublished: (event: EventTrackUnpublished) => void;
	packetsFlushedChange: (event: EventPacketsFlushedChange) => void;
	reset: () => void;
};

type OutgoingDataTrackManagerOptions = {
	e2eeManager?: BaseE2EEManager;
};

const PUBLISH_TIMEOUT_MILLISECONDS = 10_000;

export default class OutgoingDataTrackManager extends (EventEmitter as new () => TypedEmitter<DataTrackOutgoingManagerCallbacks>) {
	private e2eeManager: BaseE2EEManager | null;

	private handleAllocator = new DataTrackHandleAllocator();

	private descriptors = new Map<DataTrackHandle, Descriptor>();

	private inFlightPacketCounter = new Map<DataTrackHandle, number>();

	constructor(options?: OutgoingDataTrackManagerOptions) {
		super();
		this.e2eeManager = options?.e2eeManager ?? null;
	}

	static withDescriptors(descriptors: Map<DataTrackHandle, Descriptor>) {
		const manager = new OutgoingDataTrackManager();
		manager.descriptors = descriptors;
		return manager;
	}

	updateE2eeManager(e2eeManager: BaseE2EEManager | null) {
		this.e2eeManager = e2eeManager;

		for (const descriptor of this.descriptors.values()) {
			if (descriptor.type === 'active') {
				descriptor.pipeline.updateE2eeManager(e2eeManager);
			}
		}
	}

	getDescriptor(handle: DataTrackHandle) {
		return this.descriptors.get(handle) ?? null;
	}

	async tryProcessAndSend(
		handle: DataTrackHandle,
		frame: DataTrackFrameInternal,
	): Promise<
		Throws<
			void,
			| DataTrackPushFrameError<DataTrackPushFrameErrorReason.Dropped>
			| DataTrackPushFrameError<DataTrackPushFrameErrorReason.TrackUnpublished>
		>
	> {
		const descriptor = this.getDescriptor(handle);
		if (descriptor?.type !== 'active') {
			throw DataTrackPushFrameError.trackUnpublished();
		}

		if (descriptor.publishState === 'unpublished') {
			throw DataTrackPushFrameError.trackUnpublished();
		}
		if (descriptor.publishState === 'republishing') {
			throw DataTrackPushFrameError.dropped('Data track republishing');
		}

		try {
			for await (const packet of descriptor.pipeline.processFrame(frame)) {
				const prev = this.inFlightPacketCounter.get(handle) ?? 0;
				this.inFlightPacketCounter.set(handle, prev + 1);
				if (prev === 0) {
					this.emit('packetsFlushedChange', {handle, isFlushed: false});
				}

				this.emit('packetAvailable', {handle, bytes: packet.toBinary()});
			}
		} catch (err) {
			throw DataTrackPushFrameError.dropped(err);
		}
	}

	handlePacketSendComplete(handle: DataTrackHandle) {
		const prev = this.inFlightPacketCounter.get(handle) ?? 0;
		let counter = prev - 1;

		if (counter < 0) {
			log.warn(
				`OutgoingDataTrackManager.handlePacketSendComplete: inFlightPacketCounter was decremented below 0 (got ${this.inFlightPacketCounter} - resetting to 0. Were more packets send than were emitted?`,
			);
			counter = 0;
		}
		this.inFlightPacketCounter.set(handle, counter);

		if (counter === 0) {
			this.emit('packetsFlushedChange', {handle, isFlushed: true});
		}
	}

	async publishRequest(
		options: DataTrackOptions,
		signal?: AbortSignal,
	): Promise<Throws<DataTrackHandle, DataTrackPublishError>> {
		const handle = this.handleAllocator.get();
		if (!handle) {
			throw DataTrackPublishError.limitReached();
		}

		const timeoutSignal = abortSignalTimeout(PUBLISH_TIMEOUT_MILLISECONDS);
		const combinedSignal = signal ? abortSignalAny([signal, timeoutSignal]) : timeoutSignal;

		if (this.descriptors.has(handle)) {
			throw new Error('Descriptor for handle already exists');
		}

		const descriptor = Descriptor.pending();
		this.descriptors.set(handle, descriptor);

		const onAbort = () => {
			const existingDescriptor = this.descriptors.get(handle);
			if (!existingDescriptor) {
				log.warn(`No descriptor for ${handle}`);
				return;
			}
			this.descriptors.delete(handle);

			this.emit('sfuUnpublishRequest', {handle});

			if (existingDescriptor.type === 'pending') {
				existingDescriptor.completionFuture.reject?.(
					timeoutSignal.aborted ? DataTrackPublishError.timeout() : DataTrackPublishError.cancelled(),
				);
			}
		};
		if (combinedSignal.aborted) {
			onAbort();
			return descriptor.completionFuture.promise.then(() => handle);
		}
		combinedSignal.addEventListener('abort', onAbort);

		this.emit('sfuPublishRequest', {
			handle,
			name: options.name,
			usesE2ee: this.e2eeManager !== null,
		});

		await descriptor.completionFuture.promise;
		combinedSignal.removeEventListener('abort', onAbort);

		this.emit('trackPublished', {
			track: LocalDataTrack.withExplicitHandle(options, this, handle),
		});

		return handle;
	}

	queryPublished() {
		const descriptorInfos = Array.from(this.descriptors.values())
			.filter((descriptor): descriptor is ActiveDescriptor => descriptor.type === 'active')
			.map((descriptor) => descriptor.info);

		return descriptorInfos;
	}

	async unpublishRequest(handle: DataTrackHandle) {
		const descriptor = this.descriptors.get(handle);
		if (!descriptor) {
			log.warn(`No descriptor for ${handle}`);
			return;
		}
		if (descriptor.type !== 'active') {
			log.warn(`Track ${handle} not active`);
			return;
		}

		this.emit('sfuUnpublishRequest', {handle});

		await descriptor.unpublishingFuture.promise;

		this.inFlightPacketCounter.delete(handle);
		this.emit('trackUnpublished', {sid: descriptor.info.sid});
	}

	receivedSfuPublishResponse(handle: DataTrackHandle, result: SfuPublishResponseResult) {
		const descriptor = this.descriptors.get(handle);
		if (!descriptor) {
			log.warn(`No descriptor for ${handle}`);
			return;
		}
		this.descriptors.delete(handle);

		switch (descriptor.type) {
			case 'pending': {
				if (result.type === 'ok') {
					const info = result.data;
					log.debug(`SFU accepted publish request for handle ${handle}`, {sid: info.sid});
					const e2eeManager = info.usesE2ee ? this.e2eeManager : null;
					this.descriptors.set(info.pubHandle, Descriptor.active(info, e2eeManager));

					descriptor.completionFuture.resolve?.();
				} else {
					log.debug(`SFU rejected publish request for handle ${handle}`, {error: result.error});
					descriptor.completionFuture.reject?.(result.error);
				}
				return;
			}
			case 'active': {
				if (descriptor.publishState !== 'republishing') {
					log.warn(`Track ${handle} already active`);
					return;
				}
				if (result.type === 'error') {
					log.warn(`Republish failed for track ${handle}`);
					return;
				}

				log.debug(`Track ${handle} republished`);
				descriptor.info.sid = result.data.sid;
				descriptor.publishState = 'published';
				this.descriptors.set(descriptor.info.pubHandle, descriptor);
			}
		}
	}

	receivedSfuUnpublishResponse(handle: DataTrackHandle) {
		const descriptor = this.descriptors.get(handle);
		if (!descriptor) {
			log.warn(`No descriptor for ${handle}`);
			return;
		}
		this.descriptors.delete(handle);

		if (descriptor.type !== 'active') {
			log.warn(`Track ${handle} not active`);
			return;
		}

		descriptor.publishState = 'unpublished';
		descriptor.unpublishingFuture.resolve?.();
	}

	sfuWillRepublishTracks() {
		for (const [handle, descriptor] of this.descriptors.entries()) {
			switch (descriptor.type) {
				case 'pending':
					this.descriptors.delete(handle);
					descriptor.completionFuture.reject?.(DataTrackPublishError.disconnected());
					break;
				case 'active':
					descriptor.publishState = 'republishing';

					this.emit('sfuPublishRequest', {
						handle: descriptor.info.pubHandle,
						name: descriptor.info.name,
						usesE2ee: descriptor.info.usesE2ee,
					});
			}
		}
	}

	async reset() {
		this.handleAllocator.reset();

		for (const descriptor of this.descriptors.values()) {
			switch (descriptor.type) {
				case 'pending':
					descriptor.completionFuture.reject?.(DataTrackPublishError.disconnected());
					break;
				case 'active':
					descriptor.unpublishingFuture.resolve?.();

					await this.unpublishRequest(descriptor.info.pubHandle);
					break;
			}
		}
		this.descriptors.clear();

		this.inFlightPacketCounter.clear();

		this.emit('reset');
	}
}
