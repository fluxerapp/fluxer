// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay.module.css';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import {
	EmojiAttributionSubtext,
	getEmojiAttribution,
} from '@app/features/emoji/components/emojis/EmojiAttributionSubtext';
import Emoji from '@app/features/emoji/state/Emoji';
import {getEmojiURL} from '@app/features/expressions/utils/EmojiUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import Guilds from '@app/features/guild/state/Guilds';
import {setUrlQueryParams} from '@app/features/messaging/utils/MessagingUrlUtils';
import {usePresenceCustomStatus} from '@app/features/presence/hooks/usePresenceCustomStatus';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {EmojiTooltipContent} from '@app/features/ui/emoji_tooltip_content/EmojiTooltipContent';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useTextOverflow} from '@app/features/ui/hooks/useTextOverflow';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {HoverFloatingTooltipSurface} from '@app/features/ui/tooltip/HoverFloatingTooltipSurface';
import {HoverFloatingTooltipTrigger} from '@app/features/ui/tooltip/HoverFloatingTooltipTrigger';
import {Tooltip, type TooltipPosition} from '@app/features/ui/tooltip/Tooltip';
import {useHoverFloatingTooltip} from '@app/features/ui/tooltip/useHoverFloatingTooltip';
import {type CustomStatus, getCustomStatusText, normalizeCustomStatus} from '@app/features/user/state/CustomStatus';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {Trans} from '@lingui/react/macro';
import {PencilIcon, SmileyIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

export interface EmojiPressData {
	id: string | null;
	name: string;
	animated: boolean;
}

interface CustomStatusDisplayProps {
	className?: string;
	emojiClassName?: string;
	customStatus?: CustomStatus | null;
	userId?: string;
	showText?: boolean;
	showTooltip?: boolean;
	tooltipPosition?: TooltipPosition;
	allowJumboEmoji?: boolean;
	maxLines?: number;
	isEditable?: boolean;
	onEdit?: () => void;
	onEmojiPress?: (emoji: EmojiPressData) => void;
	constrained?: boolean;
	showPlaceholder?: boolean;
	animateOnParentHover?: boolean;
	alwaysAnimate?: boolean;
}

interface ClampedStyle extends React.CSSProperties {
	'--max-lines'?: number;
}

const sanitizeText = (text: string): string => {
	return text.replace(/[\r\n]+/g, ' ').trim();
};
const getTooltipEmojiUrl = (status: CustomStatus, animationAllowed: boolean): string | null => {
	if (status.emojiId) {
		const emoji = Emoji.getEmojiById(status.emojiId);
		const isAnimated = (emoji?.animated ?? status.emojiAnimated ?? false) && animationAllowed;
		return setUrlQueryParams(AvatarUtils.getEmojiURL({id: status.emojiId, animated: isAnimated}), {
			size: 96,
			quality: 'lossless',
		});
	}
	if (status.emojiName) {
		return getEmojiURL(status.emojiName);
	}
	return null;
};

interface StatusEmojiWithTooltipProps {
	status: CustomStatus;
	children: React.ReactNode;
	onClick?: () => void;
	isButton?: boolean;
}

const StatusEmojiWithTooltip = observer(
	({status, children, onClick, isButton = false}: StatusEmojiWithTooltipProps) => {
		const tooltip = useHoverFloatingTooltip(500);
		const emoji = status.emojiId ? Emoji.getEmojiById(status.emojiId) : null;
		const attribution = getEmojiAttribution({
			emojiId: status.emojiId,
			guildId: emoji?.guildId ?? null,
			guild: emoji?.guildId ? Guilds.getGuild(emoji.guildId) : null,
			emojiName: status.emojiName,
		});
		const getEmojiDisplayName = (): string => {
			if (status.emojiId) {
				return `:${status.emojiName}:`;
			}
			if (status.emojiName) {
				return UnicodeEmojis.convertSurrogateToName(status.emojiName, true, status.emojiName);
			}
			return '';
		};
		const emojiName = getEmojiDisplayName();
		const animationAllowed = useShouldAnimate({kind: 'custom_status_emoji', isHovering: true});
		const tooltipEmojiUrl = getTooltipEmojiUrl(status, animationAllowed);
		const TriggerComponent = isButton ? 'button' : 'span';
		const triggerProps = isButton
			? {type: 'button' as const, className: styles.emojiPressable, onClick}
			: {className: styles.emojiTooltipTrigger};
		return (
			<>
				<HoverFloatingTooltipTrigger
					tooltip={tooltip}
					data-flx="app.custom-status-display.custom-status-display.status-emoji-with-tooltip.hover-floating-tooltip-trigger"
				>
					<TriggerComponent
						data-flx="app.custom-status-display.custom-status-display.status-emoji-with-tooltip.trigger-component"
						{...triggerProps}
					>
						{children}
					</TriggerComponent>
				</HoverFloatingTooltipTrigger>
				<HoverFloatingTooltipSurface
					tooltip={tooltip}
					portalDataFlx="app.custom-status-display.custom-status-display.status-emoji-with-tooltip.floating-portal"
					presenceDataFlx="app.custom-status-display.custom-status-display.status-emoji-with-tooltip.animate-presence"
					data-flx="app.custom-status-display.custom-status-display.status-emoji-with-tooltip.div"
				>
					<EmojiTooltipContent
						emojiUrl={tooltipEmojiUrl}
						emojiAlt={status.emojiName ?? undefined}
						primaryContent={emojiName}
						subtext={
							<EmojiAttributionSubtext
								attribution={attribution}
								classes={{
									container: styles.emojiTooltipSubtext,
									guildRow: styles.emojiTooltipGuildRow,
									guildIcon: styles.emojiTooltipGuildIcon,
									guildName: styles.emojiTooltipGuildName,
									verifiedIcon: styles.emojiTooltipVerifiedIcon,
								}}
								data-flx="app.custom-status-display.custom-status-display.status-emoji-with-tooltip.emoji-attribution-subtext"
							/>
						}
						data-flx="app.custom-status-display.custom-status-display.status-emoji-with-tooltip.emoji-tooltip-content"
					/>
				</HoverFloatingTooltipSurface>
			</>
		);
	},
);

interface EmojiRenderResult {
	node: React.ReactNode;
	altText: string;
}

const renderStatusEmoji = (
	status: CustomStatus,
	emojiClassName?: string,
	animateOnParentHover?: boolean,
	alwaysAnimate?: boolean,
	animationAllowed: boolean = true,
): EmojiRenderResult | null => {
	if (status.emojiId) {
		const emoji = Emoji.getEmojiById(status.emojiId);
		const altText = `:${status.emojiName}:`;
		const isAnimated = (emoji?.animated ?? status.emojiAnimated ?? false) && animationAllowed;
		const staticUrl = setUrlQueryParams(AvatarUtils.getEmojiURL({id: status.emojiId, animated: false}), {
			size: 96,
			quality: 'lossless',
		});
		const animatedUrl = isAnimated
			? setUrlQueryParams(AvatarUtils.getEmojiURL({id: status.emojiId, animated: true}), {
					size: 96,
					quality: 'lossless',
				})
			: null;
		if (alwaysAnimate && animatedUrl) {
			return {
				node: (
					<img
						src={animatedUrl}
						alt={status.emojiName ?? undefined}
						draggable={false}
						className={clsx(styles.statusEmoji, emojiClassName)}
						data-flx="app.custom-status-display.custom-status-display.render-status-emoji.status-emoji"
					/>
				),
				altText,
			};
		}
		if (animateOnParentHover && animatedUrl) {
			return {
				node: (
					<span
						className={styles.statusEmojiWrapper}
						data-flx="app.custom-status-display.custom-status-display.render-status-emoji.status-emoji-wrapper"
					>
						<img
							src={staticUrl}
							alt={status.emojiName ?? undefined}
							draggable={false}
							className={clsx(styles.statusEmoji, styles.staticEmoji, emojiClassName)}
							data-flx="app.custom-status-display.custom-status-display.render-status-emoji.status-emoji--2"
						/>
						<img
							src={animatedUrl}
							alt={status.emojiName ?? undefined}
							draggable={false}
							className={clsx(styles.statusEmoji, styles.animatedEmoji, emojiClassName)}
							data-flx="app.custom-status-display.custom-status-display.render-status-emoji.status-emoji--3"
						/>
					</span>
				),
				altText,
			};
		}
		return {
			node: (
				<img
					src={staticUrl}
					alt={status.emojiName ?? undefined}
					draggable={false}
					className={clsx(styles.statusEmoji, emojiClassName)}
					data-flx="app.custom-status-display.custom-status-display.render-status-emoji.status-emoji--4"
				/>
			),
			altText,
		};
	}
	if (status.emojiName) {
		const altText = status.emojiName;
		const twemojiUrl = getEmojiURL(status.emojiName);
		if (!twemojiUrl) return null;
		return {
			node: (
				<img
					src={twemojiUrl}
					alt={status.emojiName}
					draggable={false}
					className={clsx(styles.statusEmoji, emojiClassName)}
					data-flx="app.custom-status-display.custom-status-display.render-status-emoji.status-emoji--5"
				/>
			),
			altText,
		};
	}
	return null;
};
export const CustomStatusDisplay = observer(
	({
		className,
		emojiClassName,
		customStatus,
		userId,
		showText = true,
		showTooltip = true,
		tooltipPosition,
		allowJumboEmoji = false,
		maxLines = 1,
		isEditable = false,
		onEdit,
		onEmojiPress,
		constrained = false,
		showPlaceholder = false,
		animateOnParentHover = false,
		alwaysAnimate = false,
	}: CustomStatusDisplayProps) => {
		const containerRef = useRef<HTMLDivElement>(null);
		const shouldFetchFromPresence = customStatus === undefined && userId !== undefined;
		const presenceStatus = usePresenceCustomStatus({
			userId: userId ?? '',
			enabled: shouldFetchFromPresence,
		});
		const status = shouldFetchFromPresence ? presenceStatus : (customStatus ?? null);
		const normalized = normalizeCustomStatus(status);
		const displayText = normalized?.text ? sanitizeText(normalized.text) : null;
		const isOverflowing = useTextOverflow(containerRef, {
			content: displayText,
			checkVertical: maxLines > 1,
			measureTextRange: true,
		});
		const animationAllowed = useShouldAnimate({
			kind: 'custom_status_emoji',
			isHovering: Boolean(animateOnParentHover) || Boolean(alwaysAnimate),
		});
		if (!normalized) {
			if (showPlaceholder && isEditable && onEdit) {
				return (
					<FocusRing offset={-2} data-flx="app.custom-status-display.custom-status-display.focus-ring">
						<button
							type="button"
							className={styles.placeholder}
							onClick={onEdit}
							data-flx="app.custom-status-display.custom-status-display.placeholder.edit.button"
						>
							<SmileyIcon
								size={remFromPx(14)}
								weight="regular"
								className={styles.placeholderIcon}
								data-flx="app.custom-status-display.custom-status-display.placeholder-icon"
							/>
							<span
								className={styles.placeholderText}
								data-flx="app.custom-status-display.custom-status-display.placeholder-text"
							>
								<Trans>Set a custom status</Trans>
							</span>
						</button>
					</FocusRing>
				);
			}
			return null;
		}
		const fullText = getCustomStatusText(normalized);
		const hasEmoji = Boolean(normalized.emojiId || normalized.emojiName);
		const hasText = Boolean(normalized.text);
		if (!hasEmoji && !hasText) {
			return null;
		}
		const emojiResult = hasEmoji
			? renderStatusEmoji(normalized, emojiClassName, animateOnParentHover, alwaysAnimate, animationAllowed)
			: null;
		const isEmojiOnly = hasEmoji && !hasText;
		const isSingleLine = maxLines === 1 && !isEmojiOnly;
		const shouldClamp = maxLines > 1 && !isEmojiOnly;
		const clampedStyle: ClampedStyle | undefined = shouldClamp ? {'--max-lines': maxLines} : undefined;
		if (isEditable && onEdit) {
			const isDesktop = !MobileLayout.enabled;
			const shouldShowEmojiTooltip = showTooltip && isDesktop && hasEmoji;
			const renderEditableEmoji = () => {
				if (!emojiResult) {
					return null;
				}
				if (shouldShowEmojiTooltip) {
					return (
						<StatusEmojiWithTooltip
							status={normalized}
							data-flx="app.custom-status-display.custom-status-display.render-editable-emoji.status-emoji-with-tooltip"
						>
							{emojiResult.node}
							<span
								className={styles.hiddenVisually}
								data-flx="app.custom-status-display.custom-status-display.render-editable-emoji.hidden-visually"
							>
								{emojiResult.altText}
							</span>
						</StatusEmojiWithTooltip>
					);
				}
				return (
					<>
						{emojiResult.node}
						<span
							className={styles.hiddenVisually}
							data-flx="app.custom-status-display.custom-status-display.render-editable-emoji.hidden-visually--2"
						>
							{emojiResult.altText}
						</span>
					</>
				);
			};
			const editableContent = (
				<FocusRing offset={-2} data-flx="app.custom-status-display.custom-status-display.focus-ring--2">
					<button
						type="button"
						className={clsx(styles.editableWrapper, {
							[styles.editableTextHover]: hasText,
							[styles.editableEmojiOnly]: isEmojiOnly,
						})}
						onClick={onEdit}
						data-flx="app.custom-status-display.custom-status-display.editable-wrapper.button"
					>
						<div
							ref={containerRef}
							className={clsx(styles.content, className, {
								[styles.jumbo]: allowJumboEmoji && isEmojiOnly,
								[styles.singleLine]: isSingleLine,
								[styles.clamped]: shouldClamp,
							})}
							style={clampedStyle}
							data-flx="app.custom-status-display.custom-status-display.content"
						>
							{renderEditableEmoji()}
							{showText && displayText && (
								<span
									className={styles.truncatedText}
									data-flx="app.custom-status-display.custom-status-display.truncated-text"
								>
									{displayText}
								</span>
							)}
						</div>
						{isEmojiOnly && (
							<PencilIcon
								size={remFromPx(12)}
								weight="bold"
								className={styles.editPencilIcon}
								data-flx="app.custom-status-display.custom-status-display.edit-pencil-icon"
							/>
						)}
					</button>
				</FocusRing>
			);
			if (showTooltip && fullText && isOverflowing) {
				return (
					<Tooltip
						text={fullText}
						position={tooltipPosition}
						data-flx="app.custom-status-display.custom-status-display.tooltip"
					>
						{editableContent}
					</Tooltip>
				);
			}
			return editableContent;
		}
		const handleEmojiPress = () => {
			if (!onEmojiPress || !normalized) {
				return;
			}
			const emoji = Emoji.getEmojiById(normalized.emojiId ?? '');
			const shouldAnimate = emoji?.animated ?? normalized.emojiAnimated ?? false;
			onEmojiPress({
				id: normalized.emojiId,
				name: normalized.emojiName ?? '',
				animated: shouldAnimate,
			});
		};
		const renderEmojiNode = () => {
			if (!emojiResult) {
				return null;
			}
			const isDesktop = !MobileLayout.enabled;
			const shouldShowEmojiTooltip = showTooltip && isDesktop && hasEmoji;
			if (onEmojiPress && hasEmoji) {
				if (shouldShowEmojiTooltip) {
					return (
						<StatusEmojiWithTooltip
							status={normalized}
							onClick={handleEmojiPress}
							isButton
							data-flx="app.custom-status-display.custom-status-display.render-emoji-node.status-emoji-with-tooltip.emoji-press"
						>
							{emojiResult.node}
							<span
								className={styles.hiddenVisually}
								data-flx="app.custom-status-display.custom-status-display.render-emoji-node.hidden-visually"
							>
								{emojiResult.altText}
							</span>
						</StatusEmojiWithTooltip>
					);
				}
				return (
					<button
						type="button"
						className={styles.emojiPressable}
						onClick={handleEmojiPress}
						data-flx="app.custom-status-display.custom-status-display.render-emoji-node.emoji-pressable.button"
					>
						{emojiResult.node}
						<span
							className={styles.hiddenVisually}
							data-flx="app.custom-status-display.custom-status-display.render-emoji-node.hidden-visually--2"
						>
							{emojiResult.altText}
						</span>
					</button>
				);
			}
			if (shouldShowEmojiTooltip) {
				return (
					<StatusEmojiWithTooltip
						status={normalized}
						data-flx="app.custom-status-display.custom-status-display.render-emoji-node.status-emoji-with-tooltip"
					>
						{emojiResult.node}
						<span
							className={styles.hiddenVisually}
							data-flx="app.custom-status-display.custom-status-display.render-emoji-node.hidden-visually--3"
						>
							{emojiResult.altText}
						</span>
					</StatusEmojiWithTooltip>
				);
			}
			return (
				<span
					className={styles.emojiTooltipTrigger}
					data-flx="app.custom-status-display.custom-status-display.render-emoji-node.emoji-tooltip-trigger"
				>
					{emojiResult.node}
					<span
						className={styles.hiddenVisually}
						data-flx="app.custom-status-display.custom-status-display.render-emoji-node.hidden-visually--4"
					>
						{emojiResult.altText}
					</span>
				</span>
			);
		};
		const content = (
			<div
				ref={containerRef}
				className={clsx(styles.content, className, {
					[styles.jumbo]: allowJumboEmoji && isEmojiOnly,
					[styles.singleLine]: isSingleLine,
					[styles.clamped]: shouldClamp,
					[styles.constrained]: constrained,
				})}
				style={clampedStyle}
				data-flx="app.custom-status-display.custom-status-display.content--2"
			>
				<span
					className={styles.hiddenVisually}
					data-flx="app.custom-status-display.custom-status-display.hidden-visually"
				>
					<Trans>Custom status: </Trans>
				</span>
				{renderEmojiNode()}
				{showText && displayText && (
					<span
						className={styles.truncatedText}
						data-flx="app.custom-status-display.custom-status-display.truncated-text--2"
					>
						{displayText}
					</span>
				)}
			</div>
		);
		if (showTooltip && fullText && isOverflowing) {
			return (
				<Tooltip
					text={fullText}
					position={tooltipPosition}
					data-flx="app.custom-status-display.custom-status-display.tooltip--2"
				>
					{content}
				</Tooltip>
			);
		}
		return content;
	},
);

CustomStatusDisplay.displayName = 'CustomStatusDisplay';
