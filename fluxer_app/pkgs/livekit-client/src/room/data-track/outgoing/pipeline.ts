// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import type {BaseE2EEManager} from '../../../e2ee/E2eeManager.ts';
import type {Throws} from '../../../utils/throws.ts';
import type {DataTrackFrameInternal} from '../frame.ts';
import {DataTrackE2eeExtension} from '../packet/extensions.ts';
import type {DataTrackPacket} from '../packet/index.ts';
import DataTrackPacketizer, {DataTrackPacketizerError} from '../packetizer.ts';
import type {DataTrackInfo} from '../types.ts';
import {DataTrackOutgoingPipelineError, type DataTrackOutgoingPipelineErrorReason} from './errors.ts';

type Options = {
	info: DataTrackInfo;
	e2eeManager: BaseE2EEManager | null;
};

export default class DataTrackOutgoingPipeline {
	private e2eeManager: BaseE2EEManager | null;

	private packetizer: DataTrackPacketizer;

	private static TRANSPORT_MTU_BYTES = 16_000;

	constructor(options: Options) {
		this.e2eeManager = options.e2eeManager;
		this.packetizer = new DataTrackPacketizer(options.info.pubHandle, DataTrackOutgoingPipeline.TRANSPORT_MTU_BYTES);
	}

	updateE2eeManager(e2eeManager: BaseE2EEManager | null) {
		this.e2eeManager = e2eeManager;
	}

	async *processFrame(
		frame: DataTrackFrameInternal,
	): Throws<AsyncGenerator<DataTrackPacket>, DataTrackOutgoingPipelineError> {
		const encryptedFrame = await this.encryptIfNeeded(frame);

		try {
			yield* this.packetizer.packetize(encryptedFrame);
		} catch (error) {
			if (error instanceof DataTrackPacketizerError) {
				throw DataTrackOutgoingPipelineError.packetizer(error);
			}
			throw error;
		}
	}

	async encryptIfNeeded(
		frame: DataTrackFrameInternal,
	): Promise<
		Throws<DataTrackFrameInternal, DataTrackOutgoingPipelineError<DataTrackOutgoingPipelineErrorReason.Encryption>>
	> {
		if (!this.e2eeManager) {
			return frame;
		}

		let encryptedResult: Awaited<ReturnType<BaseE2EEManager['encryptData']>>;
		try {
			encryptedResult = await this.e2eeManager.encryptData(frame.payload);
		} catch (err) {
			throw DataTrackOutgoingPipelineError.encryption(err);
		}

		frame.payload = encryptedResult.payload;
		frame.extensions.e2ee = new DataTrackE2eeExtension(encryptedResult.keyIndex, encryptedResult.iv);

		return frame;
	}
}
