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

import {FloatingPortal} from '@floating-ui/react';
import {msg} from '@lingui/core/macro';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {EmojiInfoBottomSheet} from '~/components/bottomsheets/EmojiInfoBottomSheet';
import {EmojiInfoContent} from '~/components/emojis/EmojiInfoContent';
import {EmojiTooltipContent} from '~/components/uikit/EmojiTooltipContent/EmojiTooltipContent';
import {useTooltipPortalRoot} from '~/components/uikit/Tooltip';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import {useReactionTooltip} from '~/hooks/useReactionTooltip';
import EmojiStore, {type Emoji} from '~/stores/EmojiStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import {shouldUseNativeEmoji} from '~/utils/EmojiUtils';
import {EmojiKind} from '../parser/types/enums';
import type {EmojiNode} from '../parser/types/nodes';
import {getEmojiRenderData} from '../utils/emoji-detector';
import type {RendererProps} from '.';

interface EmojiBottomSheetState {
	isOpen: boolean;
	emoji: {id?: string; name: string; animated?: boolean} | null;
}

interface EmojiWithTooltipProps {
	children: React.ReactElement<Record<string, unknown> & {ref?: React.Ref<HTMLElement>}>;
	emojiUrl: string | null;
	nativeEmoji?: React.ReactNode;
	emojiName: string;
	emojiForSubtext: Emoji;
}

const EmojiWithTooltip = observer(
	({children, emojiUrl, nativeEmoji, emojiName, emojiForSubtext}: EmojiWithTooltipProps) => {
		const tooltipPortalRoot = useTooltipPortalRoot();
		const {targetRef, tooltipRef, state, updatePosition, handlers, tooltipHandlers} = useReactionTooltip(500);

		const childRef = children.props.ref ?? null;
		const mergedRef = useMergeRefs([targetRef, childRef]);

		return (
			<>
				{React.cloneElement(children, {
					ref: mergedRef,
					...handlers,
				} as Record<string, unknown>)}
				{state.isOpen && (
					<FloatingPortal root={tooltipPortalRoot}>
						<AnimatePresence>
							<motion.div
								ref={(node) => {
									(tooltipRef as React.MutableRefObject<HTMLDivElement | null>).current = node;
									if (node && targetRef.current) {
										updatePosition();
									}
								}}
								style={{
									position: 'fixed',
									left: state.x,
									top: state.y,
									zIndex: 'var(--z-index-tooltip)',
									visibility: state.isReady ? 'visible' : 'hidden',
								}}
								initial={{opacity: 0, scale: 0.98}}
								animate={{opacity: 1, scale: 1}}
								exit={{opacity: 0, scale: 0.98}}
								transition={{
									opacity: {duration: 0.1},
									scale: {type: 'spring', damping: 25, stiffness: 500},
								}}
								{...tooltipHandlers}
							>
								<EmojiTooltipContent
									emoji={nativeEmoji}
									emojiUrl={emojiUrl}
									emojiAlt={emojiName}
									primaryContent={emojiName}
									subtext={<EmojiInfoContent emoji={emojiForSubtext} />}
								/>
							</motion.div>
						</AnimatePresence>
					</FloatingPortal>
				)}
			</>
		);
	},
);

const EmojiRendererInner = observer(function EmojiRendererInner({
	node,
	id,
	options,
}: RendererProps<EmojiNode>): React.ReactElement {
	const {shouldJumboEmojis, guildId, messageId, disableAnimatedEmoji} = options;
	const i18n = options.i18n!;
	const emojiData = getEmojiRenderData(node, guildId, disableAnimatedEmoji);
	const isMobile = MobileLayoutStore.enabled;

	const [bottomSheetState, setBottomSheetState] = React.useState<EmojiBottomSheetState>({
		isOpen: false,
		emoji: null,
	});

	const className = clsx('emoji', shouldJumboEmojis && 'jumboable');

	const size = shouldJumboEmojis ? 240 : 96;
	const qualitySuffix = `?size=${size}&quality=lossless`;
	const tooltipEmojiSize = 240;
	const tooltipQualitySuffix = `?size=${tooltipEmojiSize}&quality=lossless`;

	const isCustomEmoji = node.kind.kind === EmojiKind.Custom;
	const emojiRecord: Emoji | null =
		isCustomEmoji && emojiData.id ? (EmojiStore.getEmojiById(emojiData.id) ?? null) : null;
	const fallbackGuildId = emojiRecord?.guildId;
	const fallbackAnimated = emojiRecord?.animated ?? emojiData.isAnimated;

	const handleOpenBottomSheet = React.useCallback(() => {
		if (!isMobile) return;

		const emojiInfo = {
			name: node.kind.name,
			id: isCustomEmoji ? (node.kind as {id: string}).id : undefined,
			animated: isCustomEmoji ? (node.kind as {animated: boolean}).animated : false,
		};

		setBottomSheetState({isOpen: true, emoji: emojiInfo});
	}, [isMobile, node.kind, isCustomEmoji]);

	const handleCloseBottomSheet = React.useCallback(() => {
		setBottomSheetState({isOpen: false, emoji: null});
	}, []);

	const handleKeyDown = React.useCallback(
		(e: React.KeyboardEvent) => {
			if (e.key === 'Enter' || e.key === ' ') {
				handleOpenBottomSheet();
			}
		},
		[handleOpenBottomSheet],
	);

	const buildEmojiForSubtext = React.useCallback((): Emoji => {
		if (emojiRecord) {
			return emojiRecord;
		}

		return {
			id: emojiData.id,
			guildId: fallbackGuildId,
			animated: fallbackAnimated,
			name: node.kind.name,
			allNamesString: node.kind.name,
			uniqueName: node.kind.name,
		};
	}, [emojiData.id, emojiData.isAnimated, emojiRecord, node.kind.name]);

	const getTooltipData = React.useCallback(() => {
		const emojiUrl =
			shouldUseNativeEmoji && node.kind.kind === EmojiKind.Standard
				? null
				: `${emojiData.url}${emojiData.id ? tooltipQualitySuffix : ''}`;

		const nativeEmoji =
			shouldUseNativeEmoji && node.kind.kind === EmojiKind.Standard ? (
				<span className={clsx('emoji', 'jumboable')}>{node.kind.raw}</span>
			) : undefined;

		const emojiForSubtext = buildEmojiForSubtext();

		return {emojiUrl, nativeEmoji, emojiForSubtext};
	}, [emojiData, node.kind, buildEmojiForSubtext, tooltipQualitySuffix]);

	const handleImageError = (e: React.SyntheticEvent<HTMLImageElement>) => {
		const target = e.target as HTMLImageElement;
		target.style.opacity = '0.5';
		target.alt = `${emojiData.name} ${i18n._(msg`(failed to load)`)}`;
	};

	if (shouldUseNativeEmoji && node.kind.kind === EmojiKind.Standard) {
		if (isMobile) {
			return (
				<>
					<span
						className={className}
						data-message-id={messageId}
						onClick={handleOpenBottomSheet}
						onKeyDown={handleKeyDown}
						role="button"
						tabIndex={0}
					>
						{node.kind.raw}
					</span>
					<EmojiInfoBottomSheet
						isOpen={bottomSheetState.isOpen}
						onClose={handleCloseBottomSheet}
						emoji={bottomSheetState.emoji}
					/>
				</>
			);
		}

		const tooltipData = getTooltipData();
		return (
			<EmojiWithTooltip
				key={id}
				emojiUrl={tooltipData.emojiUrl}
				nativeEmoji={tooltipData.nativeEmoji}
				emojiName={emojiData.name}
				emojiForSubtext={tooltipData.emojiForSubtext}
			>
				<span className={className} data-message-id={messageId}>
					{node.kind.raw}
				</span>
			</EmojiWithTooltip>
		);
	}

	if (isMobile) {
		return (
			<>
				<span onClick={handleOpenBottomSheet} onKeyDown={handleKeyDown} role="button" tabIndex={0}>
					<img
						draggable={false}
						className={className}
						alt={emojiData.name}
						src={`${emojiData.url}${emojiData.id ? qualitySuffix : ''}`}
						data-message-id={messageId}
						data-emoji-id={emojiData.id}
						data-animated={emojiData.isAnimated}
						onError={handleImageError}
						loading="lazy"
					/>
				</span>
				<EmojiInfoBottomSheet
					isOpen={bottomSheetState.isOpen}
					onClose={handleCloseBottomSheet}
					emoji={bottomSheetState.emoji}
				/>
			</>
		);
	}

	const tooltipData = getTooltipData();
	return (
		<EmojiWithTooltip
			key={id}
			emojiUrl={tooltipData.emojiUrl}
			nativeEmoji={tooltipData.nativeEmoji}
			emojiName={emojiData.name}
			emojiForSubtext={tooltipData.emojiForSubtext}
		>
			<img
				draggable={false}
				className={className}
				alt={emojiData.name}
				src={`${emojiData.url}${emojiData.id ? qualitySuffix : ''}`}
				data-message-id={messageId}
				data-emoji-id={emojiData.id}
				data-animated={emojiData.isAnimated}
				onError={handleImageError}
				loading="lazy"
			/>
		</EmojiWithTooltip>
	);
});

export const EmojiRenderer = EmojiRendererInner;
