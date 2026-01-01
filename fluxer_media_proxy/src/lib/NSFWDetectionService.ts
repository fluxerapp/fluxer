/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import fs from 'node:fs/promises';
import path from 'node:path';
import * as ort from 'onnxruntime-node';
import sharp from 'sharp';
import {Config} from '../Config';

const MODEL_SIZE = 224;

interface NSFWCheckResult {
	isNSFW: boolean;
	probability: number;
	predictions?: {
		drawing: number;
		hentai: number;
		neutral: number;
		porn: number;
		sexy: number;
	};
}

export class NSFWDetectionService {
	private session: ort.InferenceSession | null = null;
	private readonly NSFW_THRESHOLD = 0.85;

	async initialize(): Promise<void> {
		const modelPath =
			Config.NODE_ENV === 'production' ? '/opt/data/model.onnx' : path.join(process.cwd(), 'data', 'model.onnx');

		const modelBuffer = await fs.readFile(modelPath);
		this.session = await ort.InferenceSession.create(modelBuffer);
	}

	async checkNSFW(filePath: string): Promise<NSFWCheckResult> {
		const buffer = await fs.readFile(filePath);
		return this.checkNSFWBuffer(buffer);
	}

	async checkNSFWBuffer(buffer: Buffer): Promise<NSFWCheckResult> {
		if (!this.session) {
			throw new Error('NSFW Detection service not initialized');
		}

		const processedImage = await this.preprocessImage(buffer);
		const tensor = new ort.Tensor('float32', processedImage, [1, MODEL_SIZE, MODEL_SIZE, 3]);

		const feeds = {input: tensor};
		const results = await this.session.run(feeds);

		const outputTensor = results.prediction;
		if (!outputTensor || !outputTensor.data) {
			throw new Error('ONNX model output tensor data is undefined');
		}

		const predictions = Array.from(outputTensor.data as Float32Array);

		const predictionMap = {
			drawing: predictions[0],
			// NOTE: hentai: predictions[1], gives false positives
			hentai: 0,
			neutral: predictions[2],
			porn: predictions[3],
			sexy: predictions[4],
		};

		const nsfwProbability = predictionMap.hentai + predictionMap.porn + predictionMap.sexy;

		return {
			isNSFW: nsfwProbability > this.NSFW_THRESHOLD,
			probability: nsfwProbability,
			predictions: predictionMap,
		};
	}

	private async preprocessImage(buffer: Buffer): Promise<Float32Array> {
		const imageBuffer = await sharp(buffer)
			.resize(MODEL_SIZE, MODEL_SIZE, {fit: 'fill'})
			.removeAlpha()
			.raw()
			.toBuffer();

		const float32Array = new Float32Array(MODEL_SIZE * MODEL_SIZE * 3);

		for (let i = 0; i < imageBuffer.length; i++) {
			float32Array[i] = imageBuffer[i] / 255.0;
		}

		return float32Array;
	}
}
