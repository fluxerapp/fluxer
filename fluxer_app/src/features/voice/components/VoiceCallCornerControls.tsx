// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {CallVolumeControl} from '@app/features/voice/components/CallVolumeControl';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {Icon} from '@phosphor-icons/react';
import {ArrowSquareInIcon, ArrowSquareOutIcon} from '@phosphor-icons/react';
import type React from 'react';
import {forwardRef, useMemo} from 'react';

const POP_OUT_CALL_DESCRIPTOR = msg({
	message: 'Pop out call',
	comment: 'Tooltip / aria label on the voice call footer button that opens the call in a separate window.',
});
const POP_BACK_IN_CALL_DESCRIPTOR = msg({
	message: 'Pop back in',
	comment:
		'Tooltip / aria label on the voice call footer button that restores the popped-out call into the main app window.',
});

interface VoiceCallCornerControlsProps {
	wrapClassName: string;
	buttonClassName?: string;
	showPopout: boolean;
	onPopOut?: () => void;
	showPopin?: boolean;
	onPopIn?: () => void;
	showFullscreen: boolean;
	isFullscreen: boolean;
	fullscreenLabel: string;
	fullscreenIcon: Icon;
	onToggleFullscreen: () => void;
	volumeControl?: React.ReactNode;
}

export const VoiceCallCornerControls: React.FC<VoiceCallCornerControlsProps> = ({
	wrapClassName,
	buttonClassName,
	showPopout,
	onPopOut,
	showPopin = false,
	onPopIn,
	showFullscreen,
	isFullscreen,
	fullscreenLabel,
	fullscreenIcon,
	onToggleFullscreen,
	volumeControl,
}) => {
	const {i18n} = useLingui();
	const PopOutIcon = useMemo(() => {
		const BoldIcon = forwardRef<SVGSVGElement, React.ComponentProps<typeof ArrowSquareOutIcon>>((props, ref) => (
			<ArrowSquareOutIcon
				ref={ref}
				weight="bold"
				data-flx="voice.voice-call-corner-controls.arrow-square-out-icon"
				{...props}
			/>
		));
		BoldIcon.displayName = 'PopOutCallIcon';
		return BoldIcon;
	}, []);
	const PopInIcon = useMemo(() => {
		const BoldIcon = forwardRef<SVGSVGElement, React.ComponentProps<typeof ArrowSquareInIcon>>((props, ref) => (
			<ArrowSquareInIcon
				ref={ref}
				weight="bold"
				{...props}
			/>
		));
		BoldIcon.displayName = 'PopInCallIcon';
		return BoldIcon;
	}, []);
	return (
		<div className={wrapClassName} data-flx="voice.voice-call-corner-controls.wrap">
			{volumeControl ?? (
				<CallVolumeControl
					className={buttonClassName}
					data-flx="voice.voice-call-corner-controls.call-volume-control"
				/>
			)}
			{showPopin && onPopIn ? (
				<ChannelHeaderIcon
					icon={PopInIcon}
					label={i18n._(POP_BACK_IN_CALL_DESCRIPTOR)}
					className={buttonClassName}
					onClick={onPopIn}
				/>
			) : showPopout && onPopOut ? (
				<ChannelHeaderIcon
					icon={PopOutIcon}
					label={i18n._(POP_OUT_CALL_DESCRIPTOR)}
					className={buttonClassName}
					onClick={onPopOut}
					data-flx="voice.voice-call-corner-controls.channel-header-icon.pop-out"
				/>
			) : null}
			{showFullscreen && (
				<ChannelHeaderIcon
					icon={fullscreenIcon}
					label={fullscreenLabel}
					className={buttonClassName}
					isSelected={isFullscreen}
					onClick={onToggleFullscreen}
					data-flx="voice.voice-call-corner-controls.channel-header-icon.toggle-fullscreen"
				/>
			)}
		</div>
	);
};
