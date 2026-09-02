// SPDX-License-Identifier: AGPL-3.0-or-later

import invariant from 'tiny-invariant';

const WEB_CAMERA_EFFECT_OUTPUT_FRAMES_BEHIND_SEGMENTATION_MAX = 2;

interface MutableWebCameraEffectSegmentationPhysicalOperation {
	readonly lifecycle: number;
	admittedOutputFrames: number;
	settlement: Promise<void>;
}

export class WebCameraEffectSegmentationOwner {
	private lifecycle = 0;
	private physicalOperation: MutableWebCameraEffectSegmentationPhysicalOperation | null = null;
	private deferredFailure: unknown | null = null;

	canStartPhysicalOperation(): boolean {
		return this.physicalOperation == null;
	}

	advanceLifecycle(): void {
		const nextLifecycle = this.lifecycle + 1;
		invariant(Number.isSafeInteger(nextLifecycle), 'WebGPU camera segmentation lifecycle must stay safe');
		this.lifecycle = nextLifecycle;
	}

	startPhysicalOperation(operation: Promise<void>, publishCurrentCompletion: () => void): void {
		invariant(this.physicalOperation == null, 'WebGPU camera segmentation physical operation overlapped');
		invariant(this.deferredFailure == null, 'WebGPU camera segmentation failure must be observed before restart');
		const physicalOperation: MutableWebCameraEffectSegmentationPhysicalOperation = {
			lifecycle: this.lifecycle,
			admittedOutputFrames: 0,
			settlement: Promise.resolve(),
		};
		this.physicalOperation = physicalOperation;
		physicalOperation.settlement = operation.then(
			() => this.completeSuccessfulPhysicalOperation(physicalOperation, publishCurrentCompletion),
			(error: unknown) => this.completeFailedPhysicalOperation(physicalOperation, error),
		);
	}

	admitOutputFrame(): Promise<void> | null {
		const physicalOperation = this.physicalOperation;
		if (physicalOperation == null) {
			return null;
		}
		if (physicalOperation.admittedOutputFrames < WEB_CAMERA_EFFECT_OUTPUT_FRAMES_BEHIND_SEGMENTATION_MAX) {
			physicalOperation.admittedOutputFrames += 1;
			return null;
		}
		return this.waitForOutputFrameAdmission(physicalOperation);
	}

	private async waitForOutputFrameAdmission(
		physicalOperation: MutableWebCameraEffectSegmentationPhysicalOperation,
	): Promise<void> {
		await physicalOperation.settlement;
		this.requireNoDeferredFailure();
	}

	async settlePhysicalOperation(): Promise<void> {
		const physicalOperation = this.physicalOperation;
		if (physicalOperation != null) {
			await physicalOperation.settlement;
		}
		this.requireNoDeferredFailure();
	}

	async settleForDisposal(): Promise<ReadonlyArray<unknown>> {
		const failures: Array<unknown> = [];
		const physicalOperation = this.physicalOperation;
		if (physicalOperation != null) {
			try {
				await physicalOperation.settlement;
			} catch (error) {
				failures.push(error);
			}
		}
		const failure = this.deferredFailure;
		this.deferredFailure = null;
		if (failure != null) {
			failures.push(failure);
		}
		return failures;
	}

	requireNoDeferredFailure(): void {
		const failure = this.deferredFailure;
		if (failure == null) {
			return;
		}
		this.deferredFailure = null;
		throw failure;
	}

	private completeSuccessfulPhysicalOperation(
		operation: MutableWebCameraEffectSegmentationPhysicalOperation,
		publishCurrentCompletion: () => void,
	): void {
		invariant(this.physicalOperation === operation, 'WebGPU camera segmentation physical ownership changed');
		this.physicalOperation = null;
		if (operation.lifecycle !== this.lifecycle) {
			return;
		}
		try {
			publishCurrentCompletion();
		} catch (error) {
			this.recordDeferredFailure(error);
		}
	}

	private completeFailedPhysicalOperation(
		operation: MutableWebCameraEffectSegmentationPhysicalOperation,
		failure: unknown,
	): void {
		invariant(this.physicalOperation === operation, 'WebGPU camera segmentation physical ownership changed');
		this.physicalOperation = null;
		this.recordDeferredFailure(failure);
	}

	private recordDeferredFailure(failure: unknown): void {
		let normalizedFailure = failure;
		if (normalizedFailure == null) {
			normalizedFailure = new Error('WebGPU camera segmentation failed without an error value');
		}
		invariant(this.deferredFailure == null, 'WebGPU camera segmentation owner retained multiple failures');
		this.deferredFailure = normalizedFailure;
	}
}
