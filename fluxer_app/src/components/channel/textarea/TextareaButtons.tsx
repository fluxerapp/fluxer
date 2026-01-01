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

import {useLingui} from '@lingui/react/macro';
import {GifIcon, GiftIcon, ImageSquareIcon, PaperPlaneRightIcon, SmileyIcon, StickerIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import React from 'react';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import type {ExpressionPickerTabType} from '~/components/popouts/ExpressionPickerPopout';
import {TextareaButton} from './TextareaButton';
import styles from './TextareaInput.module.css';

interface TextareaButtonsProps {
	disabled: boolean;
	showAllButtons: boolean;
	showUploadButton?: boolean;
	showGiftButton: boolean;
	showGifButton: boolean;
	showMemesButton: boolean;
	showStickersButton: boolean;
	showEmojiButton: boolean;
	showMessageSendButton: boolean;
	expressionPickerOpen: boolean;
	selectedTab: ExpressionPickerTabType;
	isMobile: boolean;
	shouldShowMobileGiftButton: boolean;
	isComposing: boolean;
	isSlowmodeActive: boolean;
	isOverLimit: boolean;
	hasContent: boolean;
	hasAttachments: boolean;
	expressionPickerTriggerRef: React.RefObject<HTMLButtonElement | null>;
	invisibleExpressionPickerTriggerRef: React.RefObject<HTMLDivElement | null>;
	onExpressionPickerToggle: (tab: ExpressionPickerTabType) => void;
	onSubmit: () => void;
	onContextMenu: (event: React.MouseEvent) => void;
	disableSendButton?: boolean;
}

export const TextareaButtons = React.forwardRef<HTMLDivElement, TextareaButtonsProps>(
	(
		{
			disabled,
			showAllButtons,
			showGiftButton,
			showGifButton,
			showMemesButton,
			showStickersButton,
			showEmojiButton,
			showMessageSendButton,
			expressionPickerOpen,
			selectedTab,
			isMobile,
			shouldShowMobileGiftButton,
			isComposing,
			isSlowmodeActive,
			isOverLimit,
			hasContent,
			hasAttachments,
			expressionPickerTriggerRef,
			invisibleExpressionPickerTriggerRef,
			onExpressionPickerToggle,
			onSubmit,
			onContextMenu,
			disableSendButton,
		},
		ref,
	) => {
		const {t} = useLingui();

		if (disabled) {
			return null;
		}

		const shouldShowDesktopSendButton = showMessageSendButton;

		return (
			<div className={clsx(styles.buttonContainerDense, styles.sideButtonPadding)} ref={ref}>
				{!isMobile && showAllButtons && (
					<>
						{showGiftButton && (
							<TextareaButton
								icon={GiftIcon}
								label={t`Gift Plutonium`}
								onClick={() => PremiumModalActionCreators.open(true)}
								onContextMenu={onContextMenu}
							/>
						)}

						{showGifButton && (
							<TextareaButton
								icon={GifIcon}
								label={t`GIFs`}
								isSelected={expressionPickerOpen && selectedTab === 'gifs'}
								onClick={() => onExpressionPickerToggle('gifs')}
								onContextMenu={onContextMenu}
								data-expression-picker-tab="gifs"
								keybindAction="toggle_gif_picker"
							/>
						)}

						{showMemesButton && (
							<TextareaButton
								icon={ImageSquareIcon}
								label={t`Media`}
								isSelected={expressionPickerOpen && selectedTab === 'memes'}
								onClick={() => onExpressionPickerToggle('memes')}
								onContextMenu={onContextMenu}
								data-expression-picker-tab="memes"
								keybindAction="toggle_memes_picker"
							/>
						)}

						{showStickersButton && (
							<TextareaButton
								icon={StickerIcon}
								label={t`Stickers`}
								isSelected={expressionPickerOpen && selectedTab === 'stickers'}
								onClick={() => onExpressionPickerToggle('stickers')}
								onContextMenu={onContextMenu}
								data-expression-picker-tab="stickers"
								keybindAction="toggle_sticker_picker"
							/>
						)}
					</>
				)}

				{showEmojiButton && (
					<TextareaButton
						ref={isMobile ? undefined : expressionPickerTriggerRef}
						icon={SmileyIcon}
						iconProps={{weight: 'fill'}}
						label={t`Emojis`}
						isSelected={expressionPickerOpen && selectedTab === 'emojis'}
						onClick={() => onExpressionPickerToggle('emojis')}
						onContextMenu={onContextMenu}
						data-expression-picker-tab="emojis"
						keybindAction="toggle_emoji_picker"
					/>
				)}

				<div
					ref={invisibleExpressionPickerTriggerRef}
					style={{position: 'absolute', pointerEvents: 'none', opacity: 0, width: 0, height: 0}}
				/>

				{isMobile && (
					<AnimatePresence initial={false}>
						{shouldShowMobileGiftButton && !isComposing && (
							<motion.div
								key="mobile-gift"
								initial={{width: 0, opacity: 0, x: 8}}
								animate={{width: 'auto', opacity: 1, x: 0}}
								exit={{width: 0, opacity: 0, x: 8}}
								transition={{type: 'tween', duration: 0.18}}
								style={{overflow: 'hidden', display: 'flex', alignItems: 'stretch'}}
							>
								<TextareaButton
									icon={GiftIcon}
									label={t`Gift Plutonium`}
									onClick={() => PremiumModalActionCreators.open(true)}
									onContextMenu={onContextMenu}
								/>
							</motion.div>
						)}
					</AnimatePresence>
				)}

				{isMobile && (
					<TextareaButton
						disabled={isSlowmodeActive || isOverLimit || (!hasContent && !hasAttachments) || disableSendButton}
						icon={PaperPlaneRightIcon}
						label={t`Send Message · Right-click to schedule`}
						onClick={onSubmit}
						onContextMenu={onContextMenu}
						keybindCombo={{key: 'Enter'}}
					/>
				)}

				{!isMobile && shouldShowDesktopSendButton && (
					<>
						<div className={styles.divider} />
						<TextareaButton
							disabled={isSlowmodeActive || isOverLimit || (!hasContent && !hasAttachments) || disableSendButton}
							icon={PaperPlaneRightIcon}
							label={t`Send Message · Right-click to schedule`}
							onClick={onSubmit}
							onContextMenu={onContextMenu}
							keybindCombo={{key: 'Enter'}}
						/>
					</>
				)}
			</div>
		);
	},
);

TextareaButtons.displayName = 'TextareaButtons';
