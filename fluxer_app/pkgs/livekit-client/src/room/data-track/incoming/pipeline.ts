// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import type {BaseE2EEManager} from '../../../e2ee/E2eeManager.ts';
import {getLogger, LoggerNames} from '../../../logger.ts';
import type {Throws} from '../../../utils/throws.ts';
import DataTrackDepacketizer, {type DataTrackDepacketizerDropError} from '../depacketizer.ts';
import type {DataTrackFrameInternal} from '../frame.ts';
import type {DataTrackPacket} from '../packet/index.ts';
import type {DataTrackInfo, RemoteDataTrackPipelineOptions} from '../types.ts';

const log = getLogger(LoggerNames.DataTracks);

type Options = {
	info: DataTrackInfo;
	publisherIdentity: string;
	e2eeManager: BaseE2EEManager | null;
	pipelineOptions?: RemoteDataTrackPipelineOptions;
};

export default class IncomingDataTrackPipeline {
	private publisherIdentity: string;

	private e2eeManager: BaseE2EEManager | null;

	private depacketizer: DataTrackDepacketizer;

	private options: RemoteDataTrackPipelineOptions;

	constructor(options: Options) {
		const hasProvider = options.e2eeManager !== null;
		if (options.info.usesE2ee !== hasProvider) {
			throw new Error('IncomingDataTrackPipeline: DataTrackInfo.usesE2ee must match presence of decryptionProvider');
		}

		const depacketizer = new DataTrackDepacketizer();

		this.publisherIdentity = options.publisherIdentity;
		this.e2eeManager = options.e2eeManager ?? null;
		this.depacketizer = depacketizer;
		this.options = options.pipelineOptions ?? {};
	}

	updateE2eeManager(e2eeManager: BaseE2EEManager | null) {
		this.e2eeManager = e2eeManager;
	}

	setOptions(options: RemoteDataTrackPipelineOptions): void {
		this.options = options;
	}

	async processPacket(
		packet: DataTrackPacket,
	): Promise<Throws<DataTrackFrameInternal | null, DataTrackDepacketizerDropError>> {
		const frame = this.depacketize(packet);
		if (!frame) {
			return null;
		}

		const decrypted = await this.decryptIfNeeded(frame);
		if (!decrypted) {
			return null;
		}

		return decrypted;
	}

	private depacketize(packet: DataTrackPacket): Throws<DataTrackFrameInternal | null, DataTrackDepacketizerDropError> {
		let frame: DataTrackFrameInternal | null;
		try {
			frame = this.depacketizer.push(packet, {
				throwOnInterruption: false,
				maxPartialFrames: this.options.maxPartialFrames,
			});
		} catch (err) {
			log.warn(`Data frame depacketize error: ${err}`);
			return null;
		}
		return frame;
	}

	private async decryptIfNeeded(frame: DataTrackFrameInternal): Promise<DataTrackFrameInternal | null> {
		const e2eeManager = this.e2eeManager;

		if (!e2eeManager) {
			return frame;
		}

		const e2ee = frame.extensions?.e2ee ?? null;
		if (!e2ee) {
			log.error('Missing E2EE meta');
			return null;
		}

		let result: Awaited<ReturnType<BaseE2EEManager['handleEncryptedData']>>;
		try {
			result = await e2eeManager.handleEncryptedData(frame.payload, e2ee.iv, this.publisherIdentity, e2ee.keyIndex);
		} catch (err) {
			log.error(`Error decrypting packet: ${err}`);
			return null;
		}

		frame.payload = result.payload;
		return frame;
	}
}
