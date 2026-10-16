// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WebCameraEffectCustomFrameSource} from '@app/features/voice/utils/camera-effects/WebCameraEffectCustomImage';
import type {
	WebCameraEffectBackend,
	WebCameraPipelineConfig,
} from '@app/features/voice/utils/camera-effects/WebCameraEffectProtocol';

export interface WebCameraEffectRenderer {
	readonly backend: WebCameraEffectBackend;
	configure(config: WebCameraPipelineConfig, customFrameSource: WebCameraEffectCustomFrameSource | null): Promise<void>;
	render(frame: VideoFrame, now: number): Promise<void>;
	dispose(): Promise<void>;
}
