// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {getStatusTypeLabel} from '@app/features/app/constants/AppConstants';
import {useAnimatedMediaPlaybackAllowed} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {type AvatarStatusLayout, getAvatarStatusLayout} from '@app/features/ui/components/AvatarStatusLayout';
import styles from '@app/features/ui/components/BaseAvatar.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {normalizeStatus, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import React, {useCallback, useEffect, useId, useMemo, useRef, useState} from 'react';

const AVATAR_DESCRIPTOR = msg({
	message: 'Avatar',
	comment: 'Accessible label for an avatar image.',
});
const TYPING_INDICATOR_DESCRIPTOR = msg({
	message: 'Typing indicator',
	comment: 'Accessible label for the typing-indicator element.',
});
const STATUS_DESCRIPTOR = msg({
	message: '{effectiveStatusLabel} status',
	comment: 'Accessible label for the avatar status badge; placeholder is the localized status name.',
});
const TYPING_DOT_GAP_RATIO = 0.12;
const TYPING_DOT_SIZE_RATIO = 0.25;
const DEFAULT_CUSTOM_STATUS_BADGE_SCALE = 1.25;
const DEFAULT_CUSTOM_STATUS_BADGE_CUTOUT_PADDING_SCALE = 1.2;
const STATUS_TRANSITION_DURATION_MS = 160;

type AvatarCSSProperties = React.CSSProperties & {
	'--avatar-status-width'?: string;
	'--avatar-status-height'?: string;
	'--avatar-status-right'?: string;
	'--avatar-status-bottom'?: string;
	'--avatar-status-radius'?: string;
	'--avatar-status-color'?: string;
	'--avatar-typing-dot-gap'?: string;
	'--avatar-typing-dot-size'?: string;
};

interface AvatarStatusBox {
	width: number;
	height: number;
	right: number;
	bottom: number;
	radius: number;
	cutoutPadding?: number;
}

const SVG_MASK_URL_CACHE = new Map<string, string>();
const AVATAR_VIEW_BOX_CACHE = new Map<number, string>();
const STATIC_AVATAR_MASK_SIZES = new Set([16, 20, 24, 32, 36, 40, 44, 48, 56, 80, 120]);
const STATUS_INDICATOR_STYLE = {display: 'block', width: '100%', height: '100%'} satisfies React.CSSProperties;

type DynamicAvatarMaskKind = 'animated' | 'mobile' | 'round' | 'typing';

interface AvatarMaskConfig {
	id: string;
	dynamicKind: DynamicAvatarMaskKind | null;
}

function getSvgMaskUrl(maskId: string): string {
	if (maskId.startsWith('svg-mask-avatar-dynamic-')) {
		return `url(#${maskId})`;
	}
	const cached = SVG_MASK_URL_CACHE.get(maskId);
	if (cached) return cached;
	const value = `url(#${maskId})`;
	SVG_MASK_URL_CACHE.set(maskId, value);
	return value;
}

function getAvatarViewBox(size: number): string {
	const cached = AVATAR_VIEW_BOX_CACHE.get(size);
	if (cached) return cached;
	const value = `0 0 ${size} ${size}`;
	AVATAR_VIEW_BOX_CACHE.set(size, value);
	return value;
}

function getDynamicAvatarMaskId(kind: DynamicAvatarMaskKind, size: number): string {
	return `svg-mask-avatar-dynamic-${kind}-${String(size).replace(/[^A-Za-z0-9_-]/g, '_')}`;
}

interface BaseAvatarProps {
	size: number;
	avatarUrl: string;
	hoverAvatarUrl?: string;
	status?: StatusType | string | null;
	shouldPlayAnimated?: boolean;
	forceAnimatedPlayback?: boolean;
	isTyping?: boolean;
	showOffline?: boolean;
	showSkeleton?: boolean;
	className?: string;
	isClickable?: boolean;
	userTag?: string;
	statusLabel?: string | null;
	disableStatusTooltip?: boolean;
	isMobileStatus?: boolean;
	animateStatusCutout?: boolean;
	customStatusBadge?: React.ReactNode;
	customStatusBadgeColor?: string;
	customStatusBadgeLabel?: string | null;
	customStatusBadgeMaskId?: string;
	customStatusBadgeScale?: number;
	customStatusBadgeMaxSizeRatio?: number;
	customStatusBadgeCutoutPaddingScale?: number;
	title?: never;
	[themeAttr: `data-flx-${string}`]: string | undefined;
}

export const BaseAvatar = React.forwardRef<HTMLDivElement, BaseAvatarProps>(
	(
		{
			size,
			avatarUrl,
			hoverAvatarUrl,
			status,
			shouldPlayAnimated = false,
			forceAnimatedPlayback = false,
			isTyping = false,
			showOffline = true,
			showSkeleton = false,
			className,
			isClickable = false,
			userTag,
			statusLabel,
			disableStatusTooltip = false,
			isMobileStatus = false,
			animateStatusCutout = false,
			customStatusBadge,
			customStatusBadgeColor,
			customStatusBadgeLabel,
			customStatusBadgeMaskId,
			customStatusBadgeScale = DEFAULT_CUSTOM_STATUS_BADGE_SCALE,
			customStatusBadgeMaxSizeRatio,
			customStatusBadgeCutoutPaddingScale = DEFAULT_CUSTOM_STATUS_BADGE_CUTOUT_PADDING_SCALE,
			...props
		},
		ref,
	) => {
		const {i18n} = useLingui();
		const animatedMediaPlaybackEnabled = Boolean((shouldPlayAnimated || forceAnimatedPlayback) && hoverAvatarUrl);
		const animatedMediaPlaybackAllowed = useAnimatedMediaPlaybackAllowed({enabled: animatedMediaPlaybackEnabled});
		const normalizedStatus = status == null ? null : normalizeStatus(status);
		const renderableStatus = resolveRenderableStatus(normalizedStatus);
		const isMobileOnline = isMobileStatus && renderableStatus === StatusTypes.ONLINE && !isTyping;
		const layout = getAvatarStatusLayout(size, isMobileOnline);
		const rawDynamicMaskId = useId();
		const dynamicAvatarMaskId = useMemo(
			() => `svg-mask-avatar-dynamic-${rawDynamicMaskId.replace(/[^A-Za-z0-9_-]/g, '_')}`,
			[rawDynamicMaskId],
		);
		const hasCustomStatusBadge = customStatusBadge != null;
		const shouldShowPresenceStatus =
			isTyping || (normalizedStatus != null && (showOffline || renderableStatus !== StatusTypes.OFFLINE));
		const shouldShowStatus = layout.supportsStatus && (shouldShowPresenceStatus || hasCustomStatusBadge);
		const shouldShowCustomStatusBadge = layout.supportsStatus && hasCustomStatusBadge && !isTyping;
		const shouldAnimateAvatarMask = animateStatusCutout && shouldShowStatus && !isMobileOnline;
		const shouldUseDynamicAvatarMask = shouldShowCustomStatusBadge || shouldAnimateAvatarMask;
		const maskIsMobileOnline = shouldShowCustomStatusBadge ? false : isMobileOnline;
		const reducedMotion = Accessibility.useReducedMotion;
		const statusAnimationProgress = useAvatarStatusAnimationProgress({
			target: isTyping ? 1 : 0,
			enabled: shouldAnimateAvatarMask && !reducedMotion,
		});
		const candidateUrl =
			animatedMediaPlaybackEnabled && animatedMediaPlaybackAllowed ? hoverAvatarUrl || '' : avatarUrl;
		const [imgError, setImgError] = useState(false);
		useEffect(() => {
			setImgError(false);
		}, [candidateUrl]);
		const handleImageError = useCallback(() => {
			setImgError(true);
		}, []);
		const showFallback = !candidateUrl || imgError;
		const avatarMask = resolveAvatarMask({
			shouldShowStatus,
			isTyping,
			isMobileOnline: maskIsMobileOnline,
			size,
			animatedMaskId: shouldUseDynamicAvatarMask ? dynamicAvatarMaskId : null,
		});
		const avatarMaskUrl = getSvgMaskUrl(avatarMask.id);
		const statusMaskId = shouldShowCustomStatusBadge
			? (customStatusBadgeMaskId ?? 'svg-mask-status-online')
			: isMobileOnline
				? STATIC_AVATAR_MASK_SIZES.has(size)
					? `svg-mask-status-online-mobile-${size}`
					: 'svg-mask-status-online-mobile'
				: `svg-mask-status-${renderableStatus}`;
		const avatarViewBox = getAvatarViewBox(size);
		const statusColor = shouldShowCustomStatusBadge
			? (customStatusBadgeColor ?? `var(--status-${renderableStatus})`)
			: `var(--status-${renderableStatus})`;
		const containerStyle = useMemo<React.CSSProperties>(
			() => ({width: remFromPx(size), height: remFromPx(size), flexShrink: 0}),
			[size],
		);
		const regularStatusBox = useMemo(
			() =>
				shouldAnimateAvatarMask && !reducedMotion
					? getInterpolatedAvatarStatusBox(layout, isMobileOnline, statusAnimationProgress)
					: getAvatarStatusBox(layout, isTyping, isMobileOnline),
			[isMobileOnline, isTyping, layout, reducedMotion, shouldAnimateAvatarMask, statusAnimationProgress],
		);
		const customStatusBox = useMemo(
			() =>
				getCustomAvatarStatusBox(
					layout,
					size,
					customStatusBadgeScale,
					customStatusBadgeMaxSizeRatio,
					customStatusBadgeCutoutPaddingScale,
				),
			[customStatusBadgeCutoutPaddingScale, customStatusBadgeMaxSizeRatio, customStatusBadgeScale, layout, size],
		);
		const statusBox = shouldShowCustomStatusBadge ? customStatusBox : regularStatusBox;
		const statusContainerStyle = useMemo<AvatarCSSProperties>(() => {
			const dotSize = Math.round(layout.innerTypingHeight * TYPING_DOT_SIZE_RATIO);
			const dotGap = Math.round(layout.innerTypingHeight * TYPING_DOT_GAP_RATIO);
			return {
				'--avatar-status-width': remFromPx(statusBox.width),
				'--avatar-status-height': remFromPx(statusBox.height),
				'--avatar-status-right': remFromPx(statusBox.right),
				'--avatar-status-bottom': remFromPx(statusBox.bottom),
				'--avatar-status-radius': remFromPx(statusBox.radius),
				'--avatar-status-color': statusColor,
				'--avatar-typing-dot-gap': remFromPx(dotGap),
				'--avatar-typing-dot-size': remFromPx(dotSize),
			};
		}, [layout, statusBox, statusColor]);
		const statusContainerClassName = `${styles.statusContainer} ${
			isTyping || disableStatusTooltip ? styles.statusTooltipDisabled : ''
		} ${reducedMotion ? styles.reducedMotion : ''} ${
			shouldAnimateAvatarMask && !reducedMotion ? styles.statusAnimationDriven : ''
		}`.trim();
		const ariaLabel = statusLabel && userTag ? `${userTag}, ${statusLabel}` : userTag || i18n._(AVATAR_DESCRIPTOR);
		const interactiveProps = useMemo(
			() =>
				isClickable
					? ({role: 'button', 'aria-label': ariaLabel, tabIndex: 0} as const)
					: ({'aria-hidden': true} as const),
			[ariaLabel, isClickable],
		);
		const effectiveStatusLabel = shouldShowCustomStatusBadge
			? (customStatusBadgeLabel ?? '')
			: statusLabel || (normalizedStatus ? getStatusTypeLabel(i18n, normalizedStatus) : '');
		const statusAriaLabel = isTyping
			? i18n._(TYPING_INDICATOR_DESCRIPTOR)
			: effectiveStatusLabel
				? i18n._(STATUS_DESCRIPTOR, {effectiveStatusLabel})
				: i18n._(AVATAR_DESCRIPTOR);
		const statusBadge = shouldShowStatus ? (
			<div
				className={statusContainerClassName}
				style={statusContainerStyle}
				role="img"
				aria-label={statusAriaLabel}
				data-flx="ui.base-avatar.status-container"
			>
				{isTyping ? (
					<div className={styles.typingBubble} data-flx="ui.base-avatar.div--2">
						<div className={styles.typingDots} data-flx="ui.base-avatar.div--3">
							<div className={styles.typingDot} data-flx="ui.base-avatar.div--4" />
							<div className={styles.typingDot} data-flx="ui.base-avatar.div--5" />
							<div className={styles.typingDot} data-flx="ui.base-avatar.div--6" />
						</div>
					</div>
				) : shouldShowCustomStatusBadge ? (
					<span className={styles.customStatusBadge} data-flx="ui.base-avatar.custom-status-badge">
						<span
							className={styles.customStatusBadgeBackground}
							aria-hidden
							data-flx="ui.base-avatar.custom-status-badge-background"
						>
							<StatusIndicatorSvg
								width={statusBox.width}
								height={statusBox.height}
								fillContainer
								statusColor={statusColor}
								statusMaskId={statusMaskId}
								data-flx="ui.base-avatar.custom-status-badge-background-svg"
							/>
						</span>
						<span
							className={styles.customStatusBadgeContent}
							aria-hidden
							data-flx="ui.base-avatar.custom-status-badge-content"
						>
							{customStatusBadge}
						</span>
					</span>
				) : (
					<StatusIndicatorSvg
						width={layout.innerStatusWidth}
						height={isMobileOnline ? layout.innerStatusHeight : layout.innerStatusWidth}
						fillContainer
						statusColor={statusColor}
						statusMaskId={statusMaskId}
						data-flx="ui.base-avatar.status-indicator-svg"
					/>
				)}
			</div>
		) : null;
		const avatarElement = (
			<div
				ref={ref}
				className={`${styles.container} ${isClickable ? styles.clickable : ''} ${className || ''}`.trim()}
				style={containerStyle}
				data-flx="ui.base-avatar.div"
				{...interactiveProps}
				{...props}
			>
				<svg
					viewBox={avatarViewBox}
					className={styles.overlay}
					aria-hidden
					role="presentation"
					data-flx="ui.base-avatar.overlay"
				>
					{avatarMask.dynamicKind && (
						<DynamicAvatarMask
							id={avatarMask.id}
							kind={avatarMask.dynamicKind}
							size={size}
							layout={layout}
							statusBox={statusBox}
							data-flx="ui.base-avatar.dynamic-avatar-mask"
						/>
					)}
					{showSkeleton ? (
						<rect
							x={0}
							y={0}
							width={size}
							height={size}
							mask={avatarMaskUrl}
							fill="var(--background-modifier-accent)"
							opacity={0.45}
							data-flx="ui.base-avatar.skeleton"
						/>
					) : showFallback ? null : (
						<image
							href={candidateUrl}
							width={size}
							height={size}
							mask={avatarMaskUrl}
							preserveAspectRatio="xMidYMid slice"
							onError={handleImageError}
							data-flx="ui.base-avatar.image"
						/>
					)}
					<rect
						className={styles.hoverOverlay}
						x={0}
						y={0}
						width={size}
						height={size}
						fill="black"
						mask={avatarMaskUrl}
						data-flx="ui.base-avatar.hover-overlay"
					/>
				</svg>
				{statusBadge &&
					(isTyping || disableStatusTooltip ? (
						statusBadge
					) : (
						<Tooltip text={effectiveStatusLabel || i18n._(AVATAR_DESCRIPTOR)} data-flx="ui.base-avatar.tooltip">
							{statusBadge}
						</Tooltip>
					))}
			</div>
		);

		if (!isClickable) {
			return avatarElement;
		}

		return (
			<FocusRing offset={-2} enabled data-flx="ui.base-avatar.focus-ring">
				{avatarElement}
			</FocusRing>
		);
	},
);

BaseAvatar.displayName = 'BaseAvatar';

const resolveRenderableStatus = (status: StatusType | null | undefined): StatusType => {
	if (status == null) return StatusTypes.OFFLINE;
	if (status === StatusTypes.INVISIBLE) return StatusTypes.OFFLINE;
	return status;
};

function getAvatarStatusBox(layout: AvatarStatusLayout, isTyping: boolean, isMobileOnline: boolean): AvatarStatusBox {
	return {
		width: isTyping ? layout.innerTypingWidth : layout.innerStatusWidth,
		height: isTyping ? layout.innerTypingHeight : layout.innerStatusHeight,
		right: isTyping ? layout.innerTypingRight : layout.innerStatusRight,
		bottom: isTyping ? layout.innerTypingBottom : layout.innerStatusBottom,
		radius: isTyping ? layout.innerTypingHeight / 2 : isMobileOnline ? 0 : layout.innerStatusHeight / 2,
	};
}

function getCustomAvatarStatusBox(
	layout: AvatarStatusLayout,
	size: number,
	badgeScale: number,
	maxSizeRatio: number | undefined,
	cutoutPaddingScale: number,
): AvatarStatusBox {
	const safeBadgeScale = Number.isFinite(badgeScale) ? Math.max(1, badgeScale) : DEFAULT_CUSTOM_STATUS_BADGE_SCALE;
	const maxBadgeSize =
		maxSizeRatio != null && Number.isFinite(maxSizeRatio) && maxSizeRatio > 0
			? size * maxSizeRatio
			: Number.POSITIVE_INFINITY;
	const safeCutoutPaddingScale = Number.isFinite(cutoutPaddingScale)
		? Math.max(1, cutoutPaddingScale)
		: DEFAULT_CUSTOM_STATUS_BADGE_CUTOUT_PADDING_SCALE;
	const baseStatusSize = Math.min(layout.innerStatusWidth, layout.innerStatusHeight);
	const badgeSize = Math.max(baseStatusSize, Math.min(baseStatusSize * safeBadgeScale, maxBadgeSize));
	const centerX = size - layout.innerStatusRight - layout.innerStatusWidth / 2;
	const centerY = size - layout.innerStatusBottom - layout.innerStatusHeight / 2;
	const baseCutoutPadding = Math.max(0, layout.cutoutRadius - baseStatusSize / 2);
	const cutoutPadding = Math.max(baseCutoutPadding * safeCutoutPaddingScale, badgeSize * 0.18);

	return {
		width: badgeSize,
		height: badgeSize,
		right: size - centerX - badgeSize / 2,
		bottom: size - centerY - badgeSize / 2,
		radius: badgeSize / 2,
		cutoutPadding,
	};
}

function getInterpolatedAvatarStatusBox(
	layout: AvatarStatusLayout,
	isMobileOnline: boolean,
	progress: number,
): AvatarStatusBox {
	const statusBox = getAvatarStatusBox(layout, false, isMobileOnline);
	const typingBox = getAvatarStatusBox(layout, true, isMobileOnline);
	return {
		width: interpolate(statusBox.width, typingBox.width, progress),
		height: interpolate(statusBox.height, typingBox.height, progress),
		right: interpolate(statusBox.right, typingBox.right, progress),
		bottom: interpolate(statusBox.bottom, typingBox.bottom, progress),
		radius: interpolate(statusBox.radius, typingBox.radius, progress),
	};
}

function interpolate(from: number, to: number, progress: number): number {
	return from + (to - from) * progress;
}

function easeOutCubic(progress: number): number {
	return 1 - (1 - progress) ** 3;
}

function useAvatarStatusAnimationProgress({target, enabled}: {target: number; enabled: boolean}): number {
	const [progress, setProgress] = useState(target);
	const progressRef = useRef(progress);
	const frameRef = useRef<number | null>(null);

	useEffect(() => {
		progressRef.current = progress;
	}, [progress]);

	useEffect(() => {
		if (frameRef.current != null) {
			cancelAnimationFrame(frameRef.current);
			frameRef.current = null;
		}

		if (!enabled) {
			progressRef.current = target;
			setProgress(target);
			return;
		}

		const from = progressRef.current;
		if (Math.abs(from - target) < 0.001) {
			progressRef.current = target;
			setProgress(target);
			return;
		}

		const startedAt = performance.now();
		const step = (timestamp: number) => {
			const elapsed = Math.min(1, (timestamp - startedAt) / STATUS_TRANSITION_DURATION_MS);
			const nextProgress = interpolate(from, target, easeOutCubic(elapsed));
			progressRef.current = nextProgress;
			setProgress(nextProgress);
			if (elapsed < 1) {
				frameRef.current = requestAnimationFrame(step);
			} else {
				frameRef.current = null;
			}
		};

		frameRef.current = requestAnimationFrame(step);
		return () => {
			if (frameRef.current != null) {
				cancelAnimationFrame(frameRef.current);
				frameRef.current = null;
			}
		};
	}, [enabled, target]);

	return enabled ? progress : target;
}

const resolveAvatarMask = ({
	shouldShowStatus,
	isTyping,
	isMobileOnline,
	size,
	animatedMaskId,
}: {
	shouldShowStatus: boolean;
	isTyping: boolean;
	isMobileOnline: boolean;
	size: number;
	animatedMaskId?: string | null;
}): AvatarMaskConfig => {
	if (!shouldShowStatus) return {id: 'svg-mask-avatar-default', dynamicKind: null};
	if (animatedMaskId && !isMobileOnline) {
		return {id: animatedMaskId, dynamicKind: 'animated'};
	}
	if (STATIC_AVATAR_MASK_SIZES.has(size)) {
		if (isTyping) return {id: `svg-mask-avatar-status-typing-${size}`, dynamicKind: null};
		if (isMobileOnline) return {id: `svg-mask-avatar-status-mobile-${size}`, dynamicKind: null};
		return {id: `svg-mask-avatar-status-round-${size}`, dynamicKind: null};
	}
	const dynamicKind = isTyping ? 'typing' : isMobileOnline ? 'mobile' : 'round';
	return {id: getDynamicAvatarMaskId(dynamicKind, size), dynamicKind};
};

interface DynamicAvatarMaskProps {
	id: string;
	kind: DynamicAvatarMaskKind;
	size: number;
	layout: AvatarStatusLayout;
	statusBox: AvatarStatusBox;
}

const DynamicAvatarMask = React.memo(function DynamicAvatarMask({
	id,
	kind,
	size,
	layout,
	statusBox,
}: DynamicAvatarMaskProps) {
	const cutoutRadius = layout.cutoutRadius;
	const cutoutY = layout.cutoutCy - cutoutRadius;
	const statusCutoutGap =
		statusBox.cutoutPadding ??
		Math.max(0, layout.cutoutRadius - Math.min(layout.innerStatusWidth, layout.innerStatusHeight) / 2);
	const animatedCutoutX = size - statusBox.right - statusBox.width - statusCutoutGap;
	const animatedCutoutY = size - statusBox.bottom - statusBox.height - statusCutoutGap;
	const animatedCutoutWidth = statusBox.width + statusCutoutGap * 2;
	const animatedCutoutHeight = statusBox.height + statusCutoutGap * 2;
	const animatedCutoutRadius = statusBox.radius + statusCutoutGap;
	const typingExtension = Math.max(0, layout.innerTypingWidth - layout.innerStatusWidth);
	const typingBridgeShift = layout.innerStatusRight - layout.innerTypingRight;
	const typingLeftCx = layout.cutoutCx - typingExtension + typingBridgeShift;
	const typingRightCx = layout.cutoutCx + typingBridgeShift;
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
				{kind === 'animated' ? (
					<rect
						fill="black"
						x={animatedCutoutX}
						y={animatedCutoutY}
						width={animatedCutoutWidth}
						height={animatedCutoutHeight}
						rx={animatedCutoutRadius}
						ry={animatedCutoutRadius}
						data-flx="ui.base-avatar.dynamic-avatar-mask.animated-cutout"
					/>
				) : kind === 'typing' ? (
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
				) : kind === 'mobile' ? (
					<rect
						fill="black"
						x={layout.cutoutCx - layout.innerStatusWidth / 2}
						y={layout.cutoutCy - layout.innerStatusHeight / 2}
						width={layout.innerStatusWidth}
						height={layout.innerStatusHeight}
						rx={Math.max(1, layout.innerStatusWidth * 0.1)}
						ry={Math.max(1, layout.innerStatusWidth * 0.1)}
						data-flx="ui.base-avatar.dynamic-avatar-mask.mobile"
					/>
				) : (
					<circle
						fill="black"
						cx={layout.cutoutCx}
						cy={layout.cutoutCy}
						r={cutoutRadius}
						data-flx="ui.base-avatar.dynamic-avatar-mask.round"
					/>
				)}
			</mask>
		</defs>
	);
});

DynamicAvatarMask.displayName = 'DynamicAvatarMask';

interface StatusIndicatorSvgProps {
	width: number;
	height: number;
	fillContainer?: boolean;
	statusColor: string;
	statusMaskId: string;
}

const StatusIndicatorSvg = React.memo(function StatusIndicatorSvg({
	width,
	height,
	fillContainer = false,
	statusColor,
	statusMaskId,
}: StatusIndicatorSvgProps) {
	return (
		<svg
			width={fillContainer ? '100%' : width}
			height={fillContainer ? '100%' : height}
			style={fillContainer ? STATUS_INDICATOR_STYLE : undefined}
			viewBox="0 0 1 1"
			preserveAspectRatio="none"
			aria-hidden
			data-flx="ui.base-avatar.status-indicator-svg.svg"
		>
			<rect
				x={0}
				y={0}
				width={1}
				height={1}
				fill={statusColor}
				mask={getSvgMaskUrl(statusMaskId)}
				data-flx="ui.base-avatar.status-indicator-svg.rect"
			/>
		</svg>
	);
});

StatusIndicatorSvg.displayName = 'StatusIndicatorSvg';
