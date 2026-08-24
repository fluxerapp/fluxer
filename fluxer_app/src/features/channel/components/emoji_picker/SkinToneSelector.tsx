// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {EMOJI_CLAP} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import * as EmojiCommands from '@app/features/emoji/commands/EmojiCommands';
import Emoji from '@app/features/emoji/state/Emoji';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {SKIN_TONE_SURROGATES} from '@fluxer/constants/src/EmojiConstants';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

interface SkinTonePickerProps {
	isOpen: boolean;
	onClose: () => void;
	skinTone: string;
}

const SkinTonePicker = observer(({isOpen, onClose, skinTone}: SkinTonePickerProps) => {
	const prefersReducedMotion = Accessibility.useReducedMotion;
	const handleSelect = (surrogate: string) => {
		EmojiCommands.setSkinTone(surrogate);
		ComponentBus.dispatch('EMOJI_PICKER_RERENDER');
		onClose();
	};
	return (
		<AnimatePresence data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker.animate-presence">
			{isOpen && (
				<motion.div
					initial={prefersReducedMotion ? {opacity: 1, height: 'auto'} : {opacity: 0, height: 0}}
					animate={{opacity: 1, height: 'auto'}}
					exit={prefersReducedMotion ? {opacity: 1, height: 'auto'} : {opacity: 0, height: 0}}
					transition={prefersReducedMotion ? {duration: 0} : undefined}
					className={styles.skinTonePickerOptions}
					data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker.skin-tone-picker-options"
				>
					{[skinTone, ...['', ...SKIN_TONE_SURROGATES].filter((surrogate) => surrogate !== skinTone)].map(
						(surrogate, index) => {
							const emojiChar = EMOJI_CLAP + surrogate;
							const emojiUrl = EmojiUtils.getEmojiURL(emojiChar);
							return (
								<motion.button
									key={surrogate || 'default'}
									type="button"
									initial={prefersReducedMotion ? {opacity: 1, scale: 1} : {opacity: 0, scale: index === 0 ? 1 : 0}}
									animate={{opacity: 1, scale: 1}}
									exit={prefersReducedMotion ? {opacity: 1, scale: 1} : {opacity: 0, scale: 0}}
									transition={prefersReducedMotion ? {duration: 0} : undefined}
									className={styles.skinTonePickerItem}
									onClick={() => handleSelect(surrogate)}
									data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker.skin-tone-picker-item.select.button"
								>
									<div
										className={styles.skinTonePickerItemImage}
										style={{backgroundImage: emojiUrl ? `url(${emojiUrl})` : undefined}}
										data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker.skin-tone-picker-item-image"
									/>
								</motion.button>
							);
						},
					)}
				</motion.div>
			)}
		</AnimatePresence>
	);
});

interface SkinTonePickerButtonProps {
	onClick: () => void;
	selectedEmojiURL: string | null;
}

const SkinTonePickerButton = observer(({onClick, selectedEmojiURL}: SkinTonePickerButtonProps) => (
	<motion.button
		type="button"
		className={styles.skinTonePickerButton}
		onClick={onClick}
		style={selectedEmojiURL ? {backgroundImage: `url(${selectedEmojiURL})`} : undefined}
		initial={{opacity: 1, scale: 1}}
		animate={{opacity: 1, scale: 1}}
		exit={{opacity: 0, scale: 0}}
		data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker-button.skin-tone-picker-button.click"
	/>
));
export const SkinToneSelector = observer(() => {
	const [isOpen, setIsOpen] = useState(false);
	const skinTone = Emoji.skinTone;
	const selectedEmojiChar = EMOJI_CLAP + skinTone;
	const selectedEmojiUrl = EmojiUtils.getEmojiURL(selectedEmojiChar);
	const selectorRef = useRef<HTMLDivElement | null>(null);
	const handleClickOutside = useCallback((event: MouseEvent) => {
		if (selectorRef.current && !selectorRef.current.contains(event.target as Node)) {
			setIsOpen(false);
		}
	}, []);
	useEffect(() => {
		if (!isOpen) {
			return undefined;
		}
		document.addEventListener('mousedown', handleClickOutside);
		return () => {
			document.removeEventListener('mousedown', handleClickOutside);
		};
	}, [handleClickOutside, isOpen]);
	return (
		<div
			ref={selectorRef}
			className={styles.skinToneSelectorContainer}
			data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-selector-container"
		>
			<SkinTonePicker
				isOpen={isOpen}
				onClose={() => setIsOpen(false)}
				skinTone={skinTone}
				data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker"
			/>
			<SkinTonePickerButton
				onClick={() => setIsOpen(true)}
				selectedEmojiURL={selectedEmojiUrl}
				data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker-button.set-is-open"
			/>
		</div>
	);
});
