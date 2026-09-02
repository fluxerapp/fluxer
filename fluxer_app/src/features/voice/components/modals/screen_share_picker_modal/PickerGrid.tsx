// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/voice/components/modals/ScreenSharePickerModal.module.css';
import {PickerCardButton} from '@app/features/voice/components/modals/screen_share_picker_modal/PickerCardButton';
import {
	logger,
	type PickerCard,
	type ScreenSharePickerTab,
} from '@app/features/voice/components/modals/screen_share_picker_modal/shared';
import {forwardRef, useCallback, useEffect, useImperativeHandle, useRef, useState} from 'react';

const MAX_DEVICE_PREVIEW_STREAMS = 4;
const DEVICE_PREVIEW_IDEAL_WIDTH = 320;
const DEVICE_PREVIEW_IDEAL_HEIGHT = 180;
const DEVICE_PREVIEW_IDEAL_FRAME_RATE = 12;
const DEVICE_PREVIEW_ACQUIRE_DEADLINE_MS = 1_500;
const DEVICE_PREVIEW_RELEASE_DEADLINE_MS = 1_500;
const EMPTY_DEVICE_PREVIEW_STREAMS: ReadonlyMap<string, MediaStream> = new Map();

interface DevicePreviewRequest {
	id: number;
	deviceIds: ReadonlyArray<string>;
}

function stopDevicePreviewStream(stream: MediaStream): void {
	for (const track of stream.getTracks()) {
		track.stop();
	}
}

function stopDevicePreviewStreams(streams: Map<string, MediaStream>): void {
	for (const stream of streams.values()) {
		stopDevicePreviewStream(stream);
	}
	streams.clear();
}

async function waitForDevicePreviewWorkerRelease(worker: Promise<void> | null): Promise<boolean> {
	if (!worker) return true;
	return new Promise((resolve) => {
		let settled = false;
		const timeoutId = globalThis.setTimeout(() => {
			settled = true;
			resolve(false);
		}, DEVICE_PREVIEW_RELEASE_DEADLINE_MS);
		void worker.then(() => {
			if (settled) return;
			settled = true;
			globalThis.clearTimeout(timeoutId);
			resolve(true);
		});
	});
}

type DevicePreviewAcquisitionResult = {kind: 'acquired'; stream: MediaStream} | {kind: 'failed'} | {kind: 'timedOut'};

interface DevicePreviewAcquisition {
	result: Promise<DevicePreviewAcquisitionResult>;
	settlement: Promise<void>;
}

function acquireDevicePreviewStream(deviceId: string): DevicePreviewAcquisition {
	if (typeof navigator === 'undefined' || !navigator.mediaDevices?.getUserMedia) {
		return {result: Promise.resolve({kind: 'failed'}), settlement: Promise.resolve()};
	}
	const acquisition = navigator.mediaDevices.getUserMedia({
		audio: false,
		video: {
			deviceId: {exact: deviceId},
			width: {ideal: DEVICE_PREVIEW_IDEAL_WIDTH},
			height: {ideal: DEVICE_PREVIEW_IDEAL_HEIGHT},
			frameRate: {ideal: DEVICE_PREVIEW_IDEAL_FRAME_RATE},
		},
	});
	let resolveResult!: (result: DevicePreviewAcquisitionResult) => void;
	const result = new Promise<DevicePreviewAcquisitionResult>((resolve) => {
		resolveResult = resolve;
	});
	let resultSettled = false;
	const timeoutId = globalThis.setTimeout(() => {
		resultSettled = true;
		logger.warn('Timed out while acquiring screen share device preview', {deviceId});
		resolveResult({kind: 'timedOut'});
	}, DEVICE_PREVIEW_ACQUIRE_DEADLINE_MS);
	const settlement = acquisition.then(
		(stream) => {
			if (resultSettled) {
				stopDevicePreviewStream(stream);
				return;
			}
			resultSettled = true;
			globalThis.clearTimeout(timeoutId);
			resolveResult({kind: 'acquired', stream});
		},
		(error) => {
			if (resultSettled) {
				logger.warn('Screen share device preview acquisition failed after its deadline', {deviceId, error});
				return;
			}
			resultSettled = true;
			globalThis.clearTimeout(timeoutId);
			logger.warn('Failed to acquire screen share device preview', {deviceId, error});
			resolveResult({kind: 'failed'});
		},
	);
	return {result, settlement};
}

function getBoundedDevicePreviewIds(
	cards: ReadonlyArray<PickerCard>,
	selectedCardId: string | null,
	visibleCardIds: ReadonlySet<string>,
): ReadonlyArray<string> {
	const deviceIds: Array<string> = [];
	const append = (deviceId: string) => {
		if (deviceIds.length >= MAX_DEVICE_PREVIEW_STREAMS || deviceIds.includes(deviceId)) return;
		deviceIds.push(deviceId);
	};
	if (selectedCardId && cards.some((card) => card.id === selectedCardId)) append(selectedCardId);
	for (const card of cards) {
		if (visibleCardIds.has(card.id)) append(card.id);
	}
	for (const card of cards) {
		append(card.id);
	}
	return deviceIds;
}

interface DevicePreviewStreamsController {
	streams: ReadonlyMap<string, MediaStream>;
	release: () => Promise<DevicePreviewReleaseResult>;
	resume: () => void;
}

export type DevicePreviewReleaseResult = 'released' | 'busy';

type PublishDevicePreviewStreams = (streams: ReadonlyMap<string, MediaStream>) => void;

class DevicePreviewWorker {
	private mounted = false;
	private enabled = false;
	private suspended = false;
	private previewDeviceIds: ReadonlyArray<string> = [];
	private request: DevicePreviewRequest = {id: 0, deviceIds: []};
	private processedRequestId = 0;
	private running = false;
	private workerPromise: Promise<void> | null = null;
	private readonly pendingAcquisitionSettlements = new Set<Promise<void>>();
	private readonly activeStreams = new Map<string, MediaStream>();

	constructor(
		private readonly publishStreams: PublishDevicePreviewStreams,
		private readonly wake: () => void,
	) {}

	mount(): void {
		this.mounted = true;
	}

	unmount(): void {
		this.mounted = false;
		this.request = {id: this.request.id + 1, deviceIds: []};
		this.stopStreams();
	}

	updateRequest(enabled: boolean, deviceIds: ReadonlyArray<string>): number {
		this.enabled = enabled;
		this.previewDeviceIds = deviceIds;
		const requestId = this.request.id + 1;
		const requestedDeviceIds = this.suspended ? [] : deviceIds;
		this.request = {id: requestId, deviceIds: requestedDeviceIds};
		this.retainStreams(requestedDeviceIds);
		this.wake();
		return requestId;
	}

	cancelRequest(requestId: number): void {
		if (this.request.id !== requestId) return;
		this.request = {id: requestId + 1, deviceIds: []};
	}

	start(): void {
		if (this.running || !this.mounted || this.processedRequestId === this.request.id) return;
		const request = this.request;
		this.processedRequestId = request.id;
		this.running = true;
		const workerPromise = this.process(request).finally(() => {
			this.running = false;
			if (this.mounted && this.processedRequestId !== this.request.id) this.wake();
		});
		this.workerPromise = workerPromise;
		void workerPromise;
	}

	async release(): Promise<DevicePreviewReleaseResult> {
		this.suspended = true;
		const requestId = this.request.id + 1;
		this.request = {id: requestId, deviceIds: []};
		this.processedRequestId = requestId;
		this.stopAndPublishEmpty();
		const pendingRelease = Promise.all(
			[this.workerPromise, ...this.pendingAcquisitionSettlements].filter(
				(settlement): settlement is Promise<void> => settlement !== null,
			),
		).then(() => undefined);
		const released = await waitForDevicePreviewWorkerRelease(pendingRelease);
		if (!released) return 'busy';
		if (this.pendingAcquisitionSettlements.size > 0) return 'busy';
		this.stopStreams();
		return 'released';
	}

	resume(): void {
		this.suspended = false;
		if (!this.mounted || !this.enabled) return;
		this.request = {id: this.request.id + 1, deviceIds: this.previewDeviceIds};
		this.wake();
	}

	private async process(request: DevicePreviewRequest): Promise<void> {
		for (const deviceId of request.deviceIds) {
			if (!this.isCurrent(request)) break;
			if (this.activeStreams.has(deviceId)) continue;
			const pendingAcquisition = acquireDevicePreviewStream(deviceId);
			this.pendingAcquisitionSettlements.add(pendingAcquisition.settlement);
			void pendingAcquisition.settlement.then(() => {
				this.pendingAcquisitionSettlements.delete(pendingAcquisition.settlement);
			});
			const acquisition = await pendingAcquisition.result;
			if (acquisition.kind === 'timedOut') break;
			if (acquisition.kind === 'failed') continue;
			const {stream} = acquisition;
			if (!this.isCurrent(request) || this.activeStreams.size >= MAX_DEVICE_PREVIEW_STREAMS) {
				stopDevicePreviewStream(stream);
				break;
			}
			this.activeStreams.set(deviceId, stream);
			this.publishStreams(new Map(this.activeStreams));
		}
	}

	private isCurrent(request: DevicePreviewRequest): boolean {
		return this.mounted && request.id === this.request.id;
	}

	private retainStreams(deviceIds: ReadonlyArray<string>): void {
		let changed = false;
		for (const [deviceId, stream] of this.activeStreams) {
			if (deviceIds.includes(deviceId)) continue;
			stopDevicePreviewStream(stream);
			this.activeStreams.delete(deviceId);
			changed = true;
		}
		if (changed && this.mounted) this.publishStreams(new Map(this.activeStreams));
	}

	private stopStreams(): void {
		stopDevicePreviewStreams(this.activeStreams);
	}

	private stopAndPublishEmpty(): void {
		this.stopStreams();
		if (this.mounted) this.publishStreams(EMPTY_DEVICE_PREVIEW_STREAMS);
	}
}

function useDevicePreviewStreams(
	cards: ReadonlyArray<PickerCard>,
	enabled: boolean,
	selectedCardId: string | null,
	visibleCardIds: ReadonlySet<string>,
): DevicePreviewStreamsController {
	const [streams, setStreams] = useState<ReadonlyMap<string, MediaStream>>(EMPTY_DEVICE_PREVIEW_STREAMS);
	const [workerWakeup, setWorkerWakeup] = useState(0);
	const workerRef = useRef<DevicePreviewWorker | null>(null);
	if (!workerRef.current) {
		workerRef.current = new DevicePreviewWorker(setStreams, () => setWorkerWakeup((current) => current + 1));
	}
	const worker = workerRef.current;
	const release = useCallback(() => worker.release(), [worker]);
	const resume = useCallback(() => worker.resume(), [worker]);
	useEffect(() => {
		worker.mount();
		return () => worker.unmount();
	}, [worker]);
	useEffect(() => {
		const deviceIds = enabled ? getBoundedDevicePreviewIds(cards, selectedCardId, visibleCardIds) : [];
		const requestId = worker.updateRequest(enabled, deviceIds);
		return () => worker.cancelRequest(requestId);
	}, [cards, enabled, selectedCardId, visibleCardIds, worker]);
	useEffect(() => {
		worker.start();
	}, [worker, workerWakeup]);
	return {streams, release, resume};
}

interface PickerGridProps {
	cards: ReadonlyArray<PickerCard>;
	activeTab: ScreenSharePickerTab;
	activeShareLabel: string;
	pendingSelectionId: string | null;
	selectedCardId: string | null;
	devicePreviewsEnabled: boolean;
	onSelect: (cardId: string) => void;
	onPreviewImageError: (cardId: string) => void;
}

export interface PickerGridHandle {
	releaseDevicePreviews: () => Promise<DevicePreviewReleaseResult>;
	resumeDevicePreviews: () => void;
}

interface VisibleDeviceCards {
	cardIds: ReadonlySet<string>;
	getCardElementRef: (cardId: string) => (element: HTMLButtonElement | null) => void;
}

function updateVisibleDeviceCardIds(
	current: ReadonlySet<string>,
	entries: ReadonlyArray<IntersectionObserverEntry>,
): ReadonlySet<string> {
	const next = new Set(current);
	for (const entry of entries) {
		const cardId = (entry.target as HTMLElement).dataset.devicePreviewCardId;
		if (!cardId) continue;
		if (entry.isIntersecting) next.add(cardId);
		else next.delete(cardId);
	}
	if (next.size === current.size && Array.from(next).every((cardId) => current.has(cardId))) return current;
	return next;
}

function useVisibleDeviceCards(cards: ReadonlyArray<PickerCard>, enabled: boolean): VisibleDeviceCards {
	const [cardIds, setCardIds] = useState<ReadonlySet<string>>(() => new Set());
	const elementsRef = useRef(new Map<string, HTMLButtonElement>());
	const refCallbacks = useRef(new Map<string, (element: HTMLButtonElement | null) => void>());
	const getCardElementRef = useCallback((cardId: string) => {
		const existing = refCallbacks.current.get(cardId);
		if (existing) return existing;
		const callback = (element: HTMLButtonElement | null) => {
			if (element) elementsRef.current.set(cardId, element);
			else {
				elementsRef.current.delete(cardId);
				refCallbacks.current.delete(cardId);
			}
		};
		refCallbacks.current.set(cardId, callback);
		return callback;
	}, []);
	useEffect(() => {
		setCardIds(new Set());
		if (!enabled || typeof IntersectionObserver === 'undefined') return;
		const observer = new IntersectionObserver(
			(entries) => setCardIds((current) => updateVisibleDeviceCardIds(current, entries)),
			{threshold: 0.05},
		);
		for (const element of elementsRef.current.values()) observer.observe(element);
		return () => observer.disconnect();
	}, [cards, enabled]);
	return {cardIds, getCardElementRef};
}

interface PickerGridCardsProps extends PickerGridProps {
	devicePreviewStreams: ReadonlyMap<string, MediaStream>;
	getCardElementRef: (cardId: string) => (element: HTMLButtonElement | null) => void;
}

function PickerGridCards({
	cards,
	activeTab,
	activeShareLabel,
	pendingSelectionId,
	selectedCardId,
	devicePreviewStreams,
	onSelect,
	onPreviewImageError,
	getCardElementRef,
}: PickerGridCardsProps) {
	const isDeviceCard = activeTab === 'devices';
	return cards.map((card) => (
		<PickerCardButton
			key={card.id}
			card={card}
			isDeviceCard={isDeviceCard}
			isPending={pendingSelectionId === card.id}
			isAnyPending={pendingSelectionId != null}
			isSelected={selectedCardId === card.id}
			devicePreviewStream={isDeviceCard ? (devicePreviewStreams.get(card.id) ?? null) : null}
			actionLabel={activeShareLabel}
			ariaLabel={`${activeShareLabel}: ${card.title}`}
			onSelect={() => onSelect(card.id)}
			onPreviewImageError={() => onPreviewImageError(card.id)}
			buttonRef={isDeviceCard ? getCardElementRef(card.id) : undefined}
			data-flx="voice.screen-share-picker-modal.picker-grid.picker-card-button.select"
		/>
	));
}

export const PickerGrid = forwardRef<PickerGridHandle, PickerGridProps>(function PickerGrid(
	{
		cards,
		activeTab,
		activeShareLabel,
		pendingSelectionId,
		selectedCardId,
		devicePreviewsEnabled,
		onSelect,
		onPreviewImageError,
	},
	ref,
) {
	const visibleCards = useVisibleDeviceCards(cards, activeTab === 'devices');
	const devicePreviews = useDevicePreviewStreams(
		cards,
		activeTab === 'devices' && devicePreviewsEnabled,
		selectedCardId,
		visibleCards.cardIds,
	);
	useImperativeHandle(
		ref,
		() => ({
			releaseDevicePreviews: devicePreviews.release,
			resumeDevicePreviews: devicePreviews.resume,
		}),
		[devicePreviews.release, devicePreviews.resume],
	);
	return (
		<div className={styles.grid} data-flx="voice.screen-share-picker-modal.grid">
			<PickerGridCards
				cards={cards}
				activeTab={activeTab}
				activeShareLabel={activeShareLabel}
				pendingSelectionId={pendingSelectionId}
				selectedCardId={selectedCardId}
				devicePreviewsEnabled={devicePreviewsEnabled}
				devicePreviewStreams={devicePreviews.streams}
				onSelect={onSelect}
				onPreviewImageError={onPreviewImageError}
				getCardElementRef={visibleCards.getCardElementRef}
				data-flx="voice.screen-share-picker-modal.picker-grid.picker-grid-cards.select"
			/>
		</div>
	);
});
