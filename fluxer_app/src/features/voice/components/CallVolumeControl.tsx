// SPDX-License-Identifier: AGPL-3.0-or-later

import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {
	callVolumePercentToSliderVolume,
	resolveCallVolumeMuteToggle,
	resolveLastNonZeroCallVolume,
	sliderVolumeToCallVolumePercent,
} from '@app/features/voice/components/CallVolumeState';
import {MediaVerticalVolumeControl} from '@app/features/voice/components/media_player/components/MediaVerticalVolumeControl';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {VOICE_VOLUME_MAX_SLIDER_VOLUME} from '@app/features/voice/utils/VoiceVolumeUtils';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useRef} from 'react';

interface CallVolumeControlProps {
	className?: string;
	position?: 'above' | 'below';
	iconSize?: number;
}

export const CallVolumeControl: React.FC<CallVolumeControlProps> = observer(function CallVolumeControl({
	className,
	position = 'above',
	iconSize = 18,
}) {
	const outputVolume = VoiceSettings.outputVolume;
	const lastNonZeroVolumeRef = useRef(resolveLastNonZeroCallVolume(outputVolume, 0));
	useEffect(() => {
		lastNonZeroVolumeRef.current = resolveLastNonZeroCallVolume(outputVolume, lastNonZeroVolumeRef.current);
	}, [outputVolume]);
	const handleVolumeChange = useCallback((volume: number) => {
		const nextPercent = sliderVolumeToCallVolumePercent(volume);
		if (nextPercent === VoiceSettings.outputVolume) return;
		VoiceSettingsCommands.update({outputVolume: nextPercent});
	}, []);
	const handleToggleMute = useCallback(() => {
		VoiceSettingsCommands.update({
			outputVolume: resolveCallVolumeMuteToggle(VoiceSettings.outputVolume, lastNonZeroVolumeRef.current),
		});
	}, []);
	return (
		<MediaVerticalVolumeControl
			volume={callVolumePercentToSliderVolume(outputVolume)}
			isMuted={outputVolume === 0}
			maxVolume={VOICE_VOLUME_MAX_SLIDER_VOLUME}
			onVolumeChange={handleVolumeChange}
			onToggleMute={handleToggleMute}
			iconSize={iconSize}
			className={className}
			position={position}
			data-flx="voice.call-volume-control.media-vertical-volume-control"
		/>
	);
});
