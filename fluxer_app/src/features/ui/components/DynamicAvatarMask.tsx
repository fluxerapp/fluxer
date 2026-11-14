// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AvatarStatusLayout} from '@app/features/ui/components/AvatarStatusLayout';
import styles from '@app/features/ui/components/BaseAvatar.module.css';
import React from 'react';

type AvatarMaskCSSProperties = React.CSSProperties & {
	'--avatar-mask-x'?: string;
	'--avatar-mask-y'?: string;
	'--avatar-mask-width'?: string;
	'--avatar-mask-height'?: string;
	'--avatar-mask-radius'?: string;
};

export interface AvatarStatusBox {
	width: number;
	height: number;
	right: number;
	bottom: number;
	radius: number;
	cutoutPadding: number;
}

export type DynamicAvatarMaskKind = 'animated' | 'mobile' | 'round' | 'typing';

interface DynamicAvatarMaskProps {
	id: string;
	kind: DynamicAvatarMaskKind;
	size: number;
	layout: AvatarStatusLayout;
	statusBox: AvatarStatusBox;
	animateShape: boolean;
	reducedMotion: boolean;
}

export const DynamicAvatarMask = React.memo(function DynamicAvatarMask({
	id,
	kind,
	size,
	layout,
	statusBox,
	animateShape,
	reducedMotion,
}: DynamicAvatarMaskProps) {
	const cutoutRadius = layout.cutoutRadius;
	const cutoutY = layout.cutoutCy - cutoutRadius;
	const statusCutoutGap = statusBox.cutoutPadding;
	const animatedCutoutX = size - statusBox.right - statusBox.width - statusCutoutGap;
	const animatedCutoutY = size - statusBox.bottom - statusBox.height - statusCutoutGap;
	const animatedCutoutWidth = statusBox.width + statusCutoutGap * 2;
	const animatedCutoutHeight = statusBox.height + statusCutoutGap * 2;
	const animatedCutoutRadius = statusBox.radius + statusCutoutGap;
	const typingExtension = Math.max(0, layout.innerTypingWidth - layout.innerStatusWidth);
	const typingBridgeShift = layout.innerStatusRight - layout.innerTypingRight;
	const typingLeftCx = layout.cutoutCx - typingExtension + typingBridgeShift;
	const typingRightCx = layout.cutoutCx + typingBridgeShift;
	const animatedCutoutClassName = animateShape
		? `${styles.animatedMaskShape} ${reducedMotion ? styles.animatedMaskReducedMotion : ''}`.trim()
		: undefined;
	const animatedCutoutStyle: AvatarMaskCSSProperties | undefined = animateShape
		? {
				'--avatar-mask-x': `${animatedCutoutX}px`,
				'--avatar-mask-y': `${animatedCutoutY}px`,
				'--avatar-mask-width': `${animatedCutoutWidth}px`,
				'--avatar-mask-height': `${animatedCutoutHeight}px`,
				'--avatar-mask-radius': `${animatedCutoutRadius}px`,
			}
		: undefined;
	let cutout: React.ReactNode;
	if (kind === 'animated') {
		cutout = (
			<rect
				fill="black"
				x={animatedCutoutX}
				y={animatedCutoutY}
				width={animatedCutoutWidth}
				height={animatedCutoutHeight}
				rx={animatedCutoutRadius}
				ry={animatedCutoutRadius}
				className={animatedCutoutClassName}
				style={animatedCutoutStyle}
				data-flx="ui.base-avatar.dynamic-avatar-mask.animated-cutout"
			/>
		);
	} else if (kind === 'typing') {
		cutout = (
			<>
				<circle
					fill="black"
					cx={typingRightCx}
					cy={layout.cutoutCy}
					r={cutoutRadius}
					data-flx="ui.base-avatar.dynamic-avatar-mask.typing-right"
				/>
				<rect
					fill="black"
					x={typingLeftCx}
					y={cutoutY}
					width={typingExtension}
					height={cutoutRadius * 2}
					data-flx="ui.base-avatar.dynamic-avatar-mask.typing-bridge"
				/>
				<circle
					fill="black"
					cx={typingLeftCx}
					cy={layout.cutoutCy}
					r={cutoutRadius}
					data-flx="ui.base-avatar.dynamic-avatar-mask.typing-left"
				/>
			</>
		);
	} else if (kind === 'mobile') {
		cutout = (
			<rect
				fill="black"
				x={layout.cutoutCx - layout.innerStatusWidth / 2 - statusCutoutGap}
				y={layout.cutoutCy - layout.innerStatusHeight / 2 - statusCutoutGap}
				width={layout.innerStatusWidth + statusCutoutGap * 2}
				height={layout.innerStatusHeight + statusCutoutGap * 2}
				rx={Math.max(1, layout.innerStatusWidth * 0.1) + statusCutoutGap}
				ry={Math.max(1, layout.innerStatusWidth * 0.1) + statusCutoutGap}
				data-flx="ui.base-avatar.dynamic-avatar-mask.mobile"
			/>
		);
	} else {
		cutout = (
			<circle
				fill="black"
				cx={layout.cutoutCx}
				cy={layout.cutoutCy}
				r={cutoutRadius}
				data-flx="ui.base-avatar.dynamic-avatar-mask.round"
			/>
		);
	}
	return (
		<defs data-flx="ui.base-avatar.dynamic-avatar-mask.defs">
			<mask
				id={id}
				maskUnits="userSpaceOnUse"
				x={0}
				y={0}
				width={size}
				height={size}
				data-flx="ui.base-avatar.dynamic-avatar-mask.mask"
			>
				<circle
					fill="white"
					cx={size / 2}
					cy={size / 2}
					r={size / 2}
					data-flx="ui.base-avatar.dynamic-avatar-mask.base"
				/>
				{cutout}
			</mask>
		</defs>
	);
});

DynamicAvatarMask.displayName = 'DynamicAvatarMask';
