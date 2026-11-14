import {EventEmitter} from 'events';
import type TypedEmitter from 'typed-emitter';
import type {BaseE2EEManager} from '../../../e2ee/E2eeManager.ts';
import {getLogger, LoggerNames} from '../../../logger.ts';
import {abortSignalAny, abortSignalTimeout} from '../../../utils/abort-signal-polyfill.ts';
import type {Throws} from '../../../utils/throws.ts';
import type Participant from '../../participant/Participant.ts';
import type RemoteParticipant from '../../participant/RemoteParticipant.ts';
import {Future} from '../../utils.ts';
import type {DataTrackDepacketizerDropError} from '../depacketizer.ts';
import {type DataTrackFrame, DataTrackFrameInternal} from '../frame.ts';
import type {DataTrackHandle} from '../handle.ts';
import {DataTrackPacket} from '../packet/index.ts';
import RemoteDataTrack from '../RemoteDataTrack.ts';
import type {DataTrackInfo, DataTrackSid, RemoteDataTrackPipelineOptions} from '../types.ts';
import {DataTrackSubscribeError} from './errors.ts';
import IncomingDataTrackPipeline from './pipeline.ts';
import type {EventSfuUpdateSubscription, EventTrackAvailable, EventTrackUnavailable} from './types.ts';

const log = getLogger(LoggerNames.DataTracks);

export type DataTrackIncomingManagerCallbacks = {
	sfuUpdateSubscription: (event: EventSfuUpdateSubscription) => void;

	trackPublished: (event: EventTrackAvailable) => void;

	trackUnpublished: (event: EventTrackUnavailable) => void;
};

type SubscriptionStateNone = {type: 'none'};
type SubscriptionStatePending = {
	type: 'pending';
	completionFuture: Future<void, DataTrackSubscribeError>;
	pendingRequestCount: number;
	cancel: () => void;
};
type SubscriptionStateActive = {
	type: 'active';
	subcriptionHandle: DataTrackHandle;
	pipeline: IncomingDataTrackPipeline;
	streamControllers: Map<ReadableStreamDefaultController<DataTrackFrame>, () => void>;
};

type SubscriptionState = SubscriptionStateNone | SubscriptionStatePending | SubscriptionStateActive;

type Descriptor<S extends SubscriptionState> = {
	info: DataTrackInfo;
	publisherIdentity: Participant['identity'];
	subscription: S;
	pipelineOptions: RemoteDataTrackPipelineOptions;
};

type IncomingDataTrackManagerOptions = {
	e2eeManager?: BaseE2EEManager;
};

const SUBSCRIBE_TIMEOUT_MILLISECONDS = 10_000;

const READABLE_STREAM_DEFAULT_BUFFER_SIZE = 16;

export default class IncomingDataTrackManager extends (EventEmitter as new () => TypedEmitter<DataTrackIncomingManagerCallbacks>) {
	private e2eeManager: BaseE2EEManager | null;

	private descriptors = new Map<DataTrackSid, Descriptor<SubscriptionState>>();

	private subscriptionHandles = new Map<DataTrackHandle, DataTrackSid>();

	constructor(options?: IncomingDataTrackManagerOptions) {
		super();
		this.e2eeManager = options?.e2eeManager ?? null;
	}

	updateE2eeManager(e2eeManager: BaseE2EEManager | null) {
		this.e2eeManager = e2eeManager;

		for (const descriptor of this.descriptors.values()) {
			if (descriptor.subscription.type === 'active') {
				descriptor.subscription.pipeline.updateE2eeManager(e2eeManager);
			}
		}
	}

	setPipelineOptions(sid: DataTrackSid, options: RemoteDataTrackPipelineOptions): void {
		const descriptor = this.descriptors.get(sid);
		if (!descriptor) {
			log.warn(`Unknown track ${sid}, cannot set pipeline options.`);
			return;
		}
		descriptor.pipelineOptions = options;
		if (descriptor.subscription.type === 'active') {
			descriptor.subscription.pipeline.setOptions(options);
		}
	}

	openSubscriptionStream(
		sid: DataTrackSid,
		signal?: AbortSignal,
		bufferSize = READABLE_STREAM_DEFAULT_BUFFER_SIZE,
	): [ReadableStream<DataTrackFrame>, Promise<Throws<void, DataTrackSubscribeError>>] {
		let streamController: ReadableStreamDefaultController<DataTrackFrame> | null = null;
		const sfuSubscriptionComplete = new Future<void, DataTrackSubscribeError>();

		const descriptor = this.descriptors.get(sid);

		const detachSignal = () => {
			signal?.removeEventListener('abort', onAbort);
		};

		const cleanup = () => {
			detachSignal();

			if (!streamController) {
				log.warn(`ReadableStream subscribed to ${sid} was not started.`);
				return;
			}
			if (!descriptor || this.descriptors.get(descriptor.info.sid) !== descriptor) {
				log.warn(`Unknown track ${sid}, skipping cancel...`);
				return;
			}
			if (descriptor.subscription.type !== 'active') {
				log.warn(`Subscription for track ${sid} is not active, skipping cancel...`);
				return;
			}

			descriptor.subscription.streamControllers.delete(streamController);

			if (descriptor.subscription.streamControllers.size === 0) {
				this.unSubscribeRequest(descriptor.info.sid);
			}
		};

		const onAbort = () => {
			if (!streamController) {
				return;
			}
			if (descriptor?.subscription.type === 'active') {
				descriptor.subscription.streamControllers.delete(streamController);
			}

			streamController.error(DataTrackSubscribeError.cancelled());
			sfuSubscriptionComplete.reject?.(DataTrackSubscribeError.cancelled());

			cleanup();
		};

		const stream = new ReadableStream<DataTrackFrame>(
			{
				start: (controller) => {
					streamController = controller;

					this.subscribeRequest(sid, signal)
						.then(async () => {
							if (!descriptor || this.descriptors.get(descriptor.info.sid) !== descriptor) {
								log.error(`Unknown track ${sid}`);
								const err = DataTrackSubscribeError.disconnected();
								controller.error(err);
								sfuSubscriptionComplete.reject?.(err);
								return;
							}
							if (descriptor.subscription.type !== 'active') {
								log.error(`Subscription for track ${sid} is not active`);
								const err = DataTrackSubscribeError.disconnected();
								controller.error(err);
								sfuSubscriptionComplete.reject?.(err);
								return;
							}

							if (signal?.aborted) {
								onAbort();
								return;
							}
							signal?.addEventListener('abort', onAbort);

							descriptor.subscription.streamControllers.set(controller, detachSignal);
							sfuSubscriptionComplete.resolve?.();
						})
						.catch((err) => {
							controller.error(err);
							sfuSubscriptionComplete.reject?.(err);
						});
				},
				cancel: () => {
					cleanup();
				},
			},
			new CountQueuingStrategy({highWaterMark: bufferSize}),
		);

		return [stream, sfuSubscriptionComplete.promise];
	}

	async subscribeRequest(sid: DataTrackSid, signal?: AbortSignal): Promise<Throws<void, DataTrackSubscribeError>> {
		const descriptor = this.descriptors.get(sid);
		if (!descriptor) {
			throw new Error('Cannot subscribe to unknown track');
		}

		const waitForCompletionFuture = async (
			currentDescriptor: Descriptor<SubscriptionState>,
			userProvidedSignal?: AbortSignal,
			timeoutSignal?: AbortSignal,
		) => {
			if (currentDescriptor.subscription.type === 'active') {
				return;
			}
			if (currentDescriptor.subscription.type !== 'pending') {
				throw new Error(`Descriptor for track ${sid} is not pending, found ${currentDescriptor.subscription.type}`);
			}

			const combinedSignal = abortSignalAny(
				[userProvidedSignal, timeoutSignal].filter((s): s is AbortSignal => typeof s !== 'undefined'),
			);

			const proxiedCompletionFuture = new Future<void, DataTrackSubscribeError>();
			currentDescriptor.subscription.completionFuture.promise
				.then(() => proxiedCompletionFuture.resolve?.())
				.catch((err) => proxiedCompletionFuture.reject?.(err));

			const onAbort = () => {
				if (currentDescriptor.subscription.type !== 'pending') {
					return;
				}
				currentDescriptor.subscription.pendingRequestCount -= 1;

				if (timeoutSignal?.aborted) {
					currentDescriptor.subscription.cancel();
					return;
				}

				if (currentDescriptor.subscription.pendingRequestCount <= 0) {
					currentDescriptor.subscription.cancel();
					return;
				}

				proxiedCompletionFuture.reject?.(DataTrackSubscribeError.cancelled());
			};

			if (combinedSignal.aborted) {
				onAbort();
			}
			combinedSignal.addEventListener('abort', onAbort);
			await proxiedCompletionFuture.promise;
			combinedSignal.removeEventListener('abort', onAbort);
		};

		switch (descriptor.subscription.type) {
			case 'none': {
				descriptor.subscription = {
					type: 'pending',
					completionFuture: new Future(),
					pendingRequestCount: 1,
					cancel: () => {
						const previousDescriptorSubscription = descriptor.subscription;
						descriptor.subscription = {type: 'none'};

						this.emit('sfuUpdateSubscription', {sid: descriptor.info.sid, subscribe: false});

						if (previousDescriptorSubscription.type === 'pending') {
							previousDescriptorSubscription.completionFuture.reject?.(
								timeoutSignal.aborted ? DataTrackSubscribeError.timeout() : DataTrackSubscribeError.cancelled(),
							);
						}
					},
				};

				this.emit('sfuUpdateSubscription', {sid, subscribe: true});

				const timeoutSignal = abortSignalTimeout(SUBSCRIBE_TIMEOUT_MILLISECONDS);

				await waitForCompletionFuture(descriptor, signal, timeoutSignal);
				return;
			}
			case 'pending': {
				descriptor.subscription.pendingRequestCount += 1;

				await waitForCompletionFuture(descriptor, signal);
				return;
			}
			case 'active': {
				return;
			}
		}
	}

	async querySubscribed() {
		const descriptorInfos = Array.from(this.descriptors.values())
			.filter(
				(descriptor): descriptor is Descriptor<SubscriptionStateActive> => descriptor.subscription.type === 'active',
			)
			.map(
				(descriptor) =>
					[descriptor.info, descriptor.publisherIdentity] as [info: DataTrackInfo, identity: Participant['identity']],
			);

		return descriptorInfos;
	}

	unSubscribeRequest(sid: DataTrackSid) {
		const descriptor = this.descriptors.get(sid);
		if (!descriptor) {
			throw new Error('Cannot subscribe to unknown track');
		}

		if (descriptor.subscription.type !== 'active') {
			log.warn(
				`Unexpected descriptor state in unSubscribeRequest, expected active, found ${descriptor.subscription?.type}`,
			);
			return;
		}

		this.closeStreamControllers(descriptor.subscription.streamControllers, sid);

		const previousDescriptorSubscription = descriptor.subscription;
		descriptor.subscription = {type: 'none'};
		this.subscriptionHandles.delete(previousDescriptorSubscription.subcriptionHandle);

		this.emit('sfuUpdateSubscription', {sid, subscribe: false});
	}

	private closeStreamControllers(streamControllers: SubscriptionStateActive['streamControllers'], sid: DataTrackSid) {
		for (const [controller, detachSignal] of streamControllers) {
			detachSignal();
			try {
				controller.close();
			} catch (err) {
				log.warn(`Failed to close readable stream for track ${sid}: ${err}`);
			}
		}
	}

	async receiveSfuPublicationUpdates(updates: Map<Participant['identity'], Array<DataTrackInfo>>) {
		if (updates.size === 0) {
			return;
		}

		const publisherParticipantToSidsInUpdate = new Map<Participant['identity'], Set<DataTrackSid>>();
		for (const [publisherIdentity, infos] of updates.entries()) {
			const sidsInUpdate = new Set<DataTrackSid>();
			for (const info of infos) {
				sidsInUpdate.add(info.sid);
				if (this.descriptors.has(info.sid)) {
					continue;
				}
				if (this.handleSidReassigned(publisherIdentity, info)) {
					continue;
				}
				await this.handleTrackPublished(publisherIdentity, info);
			}
			publisherParticipantToSidsInUpdate.set(publisherIdentity, sidsInUpdate);
		}

		for (const [publisherIdentity, sidsInUpdate] of publisherParticipantToSidsInUpdate.entries()) {
			const descriptorsForPublisher = Array.from(this.descriptors.entries())
				.filter(([_sid, descriptor]) => descriptor.publisherIdentity === publisherIdentity)
				.map(([sid]) => sid);
			const unpublishedSids = descriptorsForPublisher.filter((sid) => !sidsInUpdate.has(sid));
			for (const sid of unpublishedSids) {
				this.handleTrackUnpublished(sid);
			}
		}
	}

	async queryPublications() {
		return Array.from(this.descriptors.values()).map((descriptor) => descriptor.info);
	}

	async handleTrackPublished(publisherIdentity: Participant['identity'], info: DataTrackInfo) {
		if (this.descriptors.has(info.sid)) {
			log.error(`Existing descriptor for track ${info.sid}`);
			return;
		}
		const descriptor: Descriptor<SubscriptionStateNone> = {
			info,
			publisherIdentity,
			subscription: {type: 'none'},
			pipelineOptions: {},
		};
		this.descriptors.set(descriptor.info.sid, descriptor);

		const track = new RemoteDataTrack(descriptor.info, this, {publisherIdentity});
		this.emit('trackPublished', {track});
	}

	private handleSidReassigned(publisherIdentity: Participant['identity'], info: DataTrackInfo): boolean {
		const existingEntry = Array.from(this.descriptors.entries()).find(
			([_sid, descriptor]) =>
				descriptor.publisherIdentity === publisherIdentity && descriptor.info.pubHandle === info.pubHandle,
		);
		if (!existingEntry) {
			return false;
		}
		const [oldSid, descriptor] = existingEntry;

		const {name, usesE2ee} = descriptor.info;
		if (name !== info.name || usesE2ee !== info.usesE2ee) {
			log.warn(`Info mismatch for ${oldSid}, treating as new publication`);
			return false;
		}

		const newSid = info.sid;
		log.debug(`SID reassigned: ${oldSid} -> ${newSid}`);

		if (!this.descriptors.delete(oldSid)) {
			return false;
		}
		descriptor.info.sid = newSid;

		switch (descriptor.subscription.type) {
			case 'none':
				break;
			case 'pending':
			case 'active':
				this.emit('sfuUpdateSubscription', {sid: newSid, subscribe: true});
				break;
		}
		if (descriptor.subscription.type === 'active') {
			this.subscriptionHandles.set(descriptor.subscription.subcriptionHandle, newSid);
		}
		this.descriptors.set(newSid, descriptor);
		return true;
	}

	handleTrackUnpublished(sid: DataTrackSid) {
		const descriptor = this.descriptors.get(sid);
		if (!descriptor) {
			log.error(`Unknown track ${sid}`);
			return;
		}
		this.descriptors.delete(sid);

		if (descriptor.subscription.type === 'active') {
			this.closeStreamControllers(descriptor.subscription.streamControllers, sid);
			this.subscriptionHandles.delete(descriptor.subscription.subcriptionHandle);
		}

		this.emit('trackUnpublished', {sid, publisherIdentity: descriptor.publisherIdentity});
	}

	receivedSfuSubscriberHandles(mapping: Map<DataTrackHandle, DataTrackSid>) {
		for (const [handle, sid] of mapping.entries()) {
			this.registerSubscriberHandle(handle, sid);
		}
	}

	private registerSubscriberHandle(assignedHandle: DataTrackHandle, sid: DataTrackSid) {
		const descriptor = this.descriptors.get(sid);
		if (!descriptor) {
			log.error(`Unknown track ${sid}`);
			return;
		}
		switch (descriptor.subscription.type) {
			case 'none': {
				log.warn(`No subscription for ${sid}`);
				return;
			}
			case 'active': {
				this.subscriptionHandles.delete(descriptor.subscription.subcriptionHandle);
				descriptor.subscription.subcriptionHandle = assignedHandle;
				this.subscriptionHandles.set(assignedHandle, sid);
				return;
			}
			case 'pending': {
				log.debug(`data track subscription activated`, {sid, handle: assignedHandle});
				const pipeline = new IncomingDataTrackPipeline({
					info: descriptor.info,
					publisherIdentity: descriptor.publisherIdentity,
					e2eeManager: this.e2eeManager,
					pipelineOptions: descriptor.pipelineOptions,
				});

				const previousDescriptorSubscription = descriptor.subscription;
				descriptor.subscription = {
					type: 'active',
					subcriptionHandle: assignedHandle,
					pipeline,
					streamControllers: new Map(),
				};
				this.subscriptionHandles.set(assignedHandle, sid);

				previousDescriptorSubscription.completionFuture.resolve?.();
			}
		}
	}

	async packetReceived(bytes: Uint8Array): Promise<Throws<void, DataTrackDepacketizerDropError>> {
		let packet: DataTrackPacket;
		try {
			[packet] = DataTrackPacket.fromBinary(bytes);
		} catch (err) {
			log.error(`Failed to deserialize packet: ${err}`);
			return;
		}

		const sid = this.subscriptionHandles.get(packet.header.trackHandle);
		if (!sid) {
			log.warn(`Unknown subscriber handle ${packet.header.trackHandle}`);
			return;
		}

		const descriptor = this.descriptors.get(sid);
		if (!descriptor) {
			log.error(`Missing descriptor for track ${sid}`);
			return;
		}

		if (descriptor.subscription.type !== 'active') {
			log.warn(`Received packet for track ${sid} without active subscription`);
			return;
		}

		const internalFrame = await descriptor.subscription.pipeline.processPacket(packet);
		if (!internalFrame) {
			return;
		}

		for (const controller of descriptor.subscription.streamControllers.keys()) {
			if (controller.desiredSize !== null && controller.desiredSize <= 0) {
				log.warn(
					`Cannot send frame to subscribers: readable stream is full (desiredSize is ${controller.desiredSize}). To increase this threshold, set a higher 'options.highWaterMark' when calling .subscribe().`,
				);
				continue;
			}
			const frame = DataTrackFrameInternal.lossyIntoFrame(internalFrame);
			controller.enqueue(frame);
		}
	}

	resendSubscriptionUpdates() {
		for (const [sid, descriptor] of this.descriptors) {
			if (descriptor.subscription.type === 'none') {
				continue;
			}
			this.emit('sfuUpdateSubscription', {sid, subscribe: true});
		}
	}

	handleRemoteParticipantDisconnected(remoteParticipantIdentity: RemoteParticipant['identity']) {
		for (const descriptor of this.descriptors.values()) {
			if (descriptor.publisherIdentity !== remoteParticipantIdentity) {
				continue;
			}
			switch (descriptor.subscription.type) {
				case 'none':
					break;
				case 'pending':
					descriptor.subscription.completionFuture.reject?.(DataTrackSubscribeError.disconnected());
					break;
				case 'active':
					this.unSubscribeRequest(descriptor.info.sid);
					break;
			}
		}
	}

	reset() {
		for (const descriptor of this.descriptors.values()) {
			this.emit('trackUnpublished', {
				sid: descriptor.info.sid,
				publisherIdentity: descriptor.publisherIdentity,
			});

			if (descriptor.subscription.type === 'pending') {
				descriptor.subscription.completionFuture.reject?.(DataTrackSubscribeError.disconnected());
			}

			if (descriptor.subscription.type === 'active') {
				this.closeStreamControllers(descriptor.subscription.streamControllers, descriptor.info.sid);
			}
		}
		this.descriptors.clear();
		this.subscriptionHandles.clear();
	}
}
