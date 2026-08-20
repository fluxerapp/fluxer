// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {transitionMobileTextareaButtonState} from '@app/features/channel/components/textarea/MobileTextareaButtonStateMachine';
import {TextareaButton} from '@app/features/channel/components/textarea/TextareaButton';
import textareaButtonsStyles from '@app/features/channel/components/textarea/TextareaButtons.module.css';
import styles from '@app/features/channel/components/textarea/TextareaInput.module.css';
import VoiceMessageRecorder from '@app/features/channel/components/VoiceMessageRecorder';
import type {ExpressionPickerTabType} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {
	EMOJIS_DESCRIPTOR,
	GIFS_DESCRIPTOR,
	MEDIA_DESCRIPTOR,
	STICKERS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getReducedMotionProps, type MotionAnimation} from '@app/features/ui/utils/ReducedMotionAnimation';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GifIcon, ImageSquareIcon, PaperPlaneRightIcon, SmileyIcon, StickerIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import React, {useMemo} from 'react';

const SEND_MESSAGE_DESCRIPTOR = msg({
	message: 'Send message',
	comment: 'Button or menu action label in the channel and chat textarea buttons. Keep it concise.',
});
const MOBILE_BUTTON_SWAP_MOTION: MotionAnimation = {
	initial: {opacity: 0, scale: 0.86, y: 3},
	animate: {opacity: 1, scale: 1, y: 0},
	exit: {opacity: 0, scale: 0.9, y: -2},
	transition: {
		opacity: {duration: 0.08},
		y: {duration: 0.08, ease: 'easeOut'},
		scale: {type: 'spring', stiffness: 560, damping: 28, mass: 0.55},
	},
};

interface TextareaButtonsProps {
	disabled: boolean;
	showAllButtons: boolean;
	showGifButton: boolean;
	showMemesButton: boolean;
	showStickersButton: boolean;
	showEmojiButton: boolean;
	showMessageSendButton: boolean;
	canRecordVoice: boolean;
	isEditingMessage: boolean;
	hasPendingSticker: boolean;
	voiceTooltipAnchorRef?: React.RefObject<HTMLElement | null>;
	channelId: string;
	expressionPickerOpen: boolean;
	selectedTab: ExpressionPickerTabType;
	isMobile: boolean;
	isSlowmodeActive: boolean;
	isOverLimit: boolean;
	hasContent: boolean;
	hasAttachments: boolean;
	expressionPickerTriggerRef: React.RefObject<HTMLButtonElement | null>;
	invisibleExpressionPickerTriggerRef: React.RefObject<HTMLDivElement | null>;
	onExpressionPickerToggle: (tab: ExpressionPickerTabType) => void;
	onSubmit: () => void;
	disableSendButton?: boolean;
}

export const TextareaButtons = React.forwardRef<HTMLDivElement, TextareaButtonsProps>(
	(
		{
			disabled,
			showAllButtons,
			showGifButton,
			showMemesButton,
			showStickersButton,
			showEmojiButton,
			showMessageSendButton,
			canRecordVoice,
			isEditingMessage,
			hasPendingSticker,
			voiceTooltipAnchorRef,
			channelId,
			expressionPickerOpen,
			selectedTab,
			isMobile,
			isSlowmodeActive,
			isOverLimit,
			hasContent,
			hasAttachments,
			expressionPickerTriggerRef,
			invisibleExpressionPickerTriggerRef,
			onExpressionPickerToggle,
			onSubmit,
			disableSendButton,
		},
		ref,
	) => {
		const {i18n} = useLingui();
		const buttonModel = useMemo(
			() =>
				transitionMobileTextareaButtonState({
					disabled,
					canRecordVoice,
					value: '',
					isSlowmodeActive,
					isOverCharacterLimit: isOverLimit,
					isEditingMessage,
					hasContent,
					hasAttachments,
					hasPendingSticker,
					isEditingScheduledMessage: false,
				}),
			[
				canRecordVoice,
				disabled,
				hasAttachments,
				hasContent,
				hasPendingSticker,
				isEditingMessage,
				isOverLimit,
				isSlowmodeActive,
			],
		);
		if (disabled) {
			return null;
		}
		const buttonSwapMotion = getReducedMotionProps(MOBILE_BUTTON_SWAP_MOTION, Accessibility.useReducedMotion);
		const shouldShowDesktopSendButton = showMessageSendButton;
		const desktopSendDisabled =
			isSlowmodeActive || isOverLimit || (!hasContent && !hasAttachments) || disableSendButton;
		const shouldShowVoiceButton = buttonModel.visibleButton === 'voice';
		const mobileSendDisabled = buttonModel.sendButton.disabled || Boolean(disableSendButton);
		return (
			<div
				className={clsx(styles.buttonContainerDense, styles.sideButtonPadding)}
				ref={ref}
				data-flx="channel.textarea.textarea-buttons.button-container-dense"
			>
				{!isMobile && showAllButtons && (
					<>
						{showGifButton && (
							<TextareaButton
								icon={GifIcon}
								label={i18n._(GIFS_DESCRIPTOR)}
								isSelected={expressionPickerOpen && selectedTab === 'gifs'}
								onClick={() => onExpressionPickerToggle('gifs')}
								data-expression-picker-tab="gifs"
								keybindAction="chat_toggle_gif"
								data-flx="channel.textarea.textarea-buttons.textarea-button.expression-picker-toggle"
							/>
						)}
						{showMemesButton && (
							<TextareaButton
								icon={ImageSquareIcon}
								label={i18n._(MEDIA_DESCRIPTOR)}
								isSelected={expressionPickerOpen && selectedTab === 'memes'}
								onClick={() => onExpressionPickerToggle('memes')}
								data-expression-picker-tab="memes"
								keybindAction="chat_toggle_saved_media"
								data-flx="channel.textarea.textarea-buttons.textarea-button.expression-picker-toggle--2"
							/>
						)}
						{showStickersButton && (
							<TextareaButton
								icon={StickerIcon}
								label={i18n._(STICKERS_DESCRIPTOR)}
								isSelected={expressionPickerOpen && selectedTab === 'stickers'}
								onClick={() => onExpressionPickerToggle('stickers')}
								data-expression-picker-tab="stickers"
								keybindAction="chat_toggle_sticker"
								data-flx="channel.textarea.textarea-buttons.textarea-button.expression-picker-toggle--3"
							/>
						)}
					</>
				)}
				{showEmojiButton && (
					<TextareaButton
						ref={isMobile ? undefined : expressionPickerTriggerRef}
						icon={SmileyIcon}
						iconProps={{weight: 'fill'}}
						label={i18n._(EMOJIS_DESCRIPTOR)}
						isSelected={expressionPickerOpen && selectedTab === 'emojis'}
						onClick={() => onExpressionPickerToggle('emojis')}
						data-expression-picker-tab="emojis"
						keybindAction="chat_toggle_emoji"
						data-flx="channel.textarea.textarea-buttons.textarea-button.expression-picker-toggle--4"
					/>
				)}
				<div
					ref={invisibleExpressionPickerTriggerRef}
					className={textareaButtonsStyles.invisibleTrigger}
					data-flx="channel.textarea.textarea-buttons.div"
				/>
				{isMobile && (
					<div
						className={textareaButtonsStyles.mobileRightButtonContainer}
						data-flx="channel.textarea.textarea-buttons.mobile-right-button-container"
					>
						<AnimatePresence initial={false} data-flx="channel.textarea.textarea-buttons.animate-presence">
							{shouldShowVoiceButton ? (
								<motion.div
									key="voice-button"
									className={textareaButtonsStyles.mobileRightButtonSlot}
									data-flx="channel.textarea.textarea-buttons.div--2"
									{...buttonSwapMotion}
								>
									<VoiceMessageRecorder
										channelId={channelId}
										disabled={buttonModel.voiceButton.disabled}
										tooltipAnchorRef={voiceTooltipAnchorRef}
										data-flx="channel.textarea.textarea-buttons.voice-message-recorder"
									/>
								</motion.div>
							) : (
								<motion.div
									key="send-button"
									className={textareaButtonsStyles.mobileRightButtonSlot}
									data-flx="channel.textarea.textarea-buttons.div--3"
									{...buttonSwapMotion}
								>
									<TextareaButton
										disabled={mobileSendDisabled}
										icon={PaperPlaneRightIcon}
										label={i18n._(SEND_MESSAGE_DESCRIPTOR)}
										onClick={onSubmit}
										keybindCombo={{key: 'Enter'}}
										data-flx="channel.textarea.textarea-buttons.textarea-button.submit"
									/>
								</motion.div>
							)}
						</AnimatePresence>
					</div>
				)}
				{!isMobile && shouldShowDesktopSendButton && (
					<>
						<div className={styles.divider} data-flx="channel.textarea.textarea-buttons.divider" />
						<TextareaButton
							disabled={desktopSendDisabled}
							icon={PaperPlaneRightIcon}
							label={i18n._(SEND_MESSAGE_DESCRIPTOR)}
							onClick={onSubmit}
							keybindCombo={{key: 'Enter'}}
							data-flx="channel.textarea.textarea-buttons.textarea-button.submit--2"
						/>
					</>
				)}
			</div>
		);
	},
);

TextareaButtons.displayName = 'TextareaButtons';
