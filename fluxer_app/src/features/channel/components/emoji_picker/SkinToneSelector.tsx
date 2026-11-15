// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {EMOJI_CLAP} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';

import Emoji from '@app/features/emoji/state/Emoji';
import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import {SKIN_TONE_SURROGATES} from '@fluxer/constants/src/EmojiConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const SKIN_TONE_DESCRIPTOR = msg({
	message: 'Skin tone',
	comment: 'Accessible name of the emoji picker button that opens the skin tone choices.',
});
const SKIN_TONE_LABELS: Readonly<Record<string, MessageDescriptor>> = {
	'': msg({message: 'Default skin tone', comment: 'Accessible name of a skin tone choice in the emoji picker.'}),
	'🏻': msg({message: 'Light skin tone', comment: 'Accessible name of a skin tone choice in the emoji picker.'}),
	'🏼': msg({message: 'Medium-light skin tone', comment: 'Accessible name of a skin tone choice in the emoji picker.'}),
	'🏽': msg({message: 'Medium skin tone', comment: 'Accessible name of a skin tone choice in the emoji picker.'}),
	'🏾': msg({message: 'Medium-dark skin tone', comment: 'Accessible name of a skin tone choice in the emoji picker.'}),
	'🏿': msg({message: 'Dark skin tone', comment: 'Accessible name of a skin tone choice in the emoji picker.'}),
};

interface SkinTonePickerProps {
	isOpen: boolean;
	onClose: () => void;
	skinTone: string;
}

const SkinTonePicker = observer(({isOpen, onClose, skinTone}: SkinTonePickerProps) => {
	const {i18n} = useLingui();
	const prefersReducedMotion = Accessibility.useReducedMotion;
	const handleSelect = (surrogate: string) => {
		Emoji.setSkinTone(surrogate);
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
									aria-label={i18n._(SKIN_TONE_LABELS[surrogate] ?? SKIN_TONE_DESCRIPTOR)}
									aria-pressed={surrogate === skinTone}
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
	expanded: boolean;
}

const SkinTonePickerButton = observer(({onClick, selectedEmojiURL, expanded}: SkinTonePickerButtonProps) => {
	const {i18n} = useLingui();
	return (
		<motion.button
			type="button"
			className={styles.skinTonePickerButton}
			aria-label={i18n._(SKIN_TONE_DESCRIPTOR)}
			aria-expanded={expanded}
			onClick={onClick}
			style={selectedEmojiURL ? {backgroundImage: `url(${selectedEmojiURL})`} : undefined}
			initial={{opacity: 1, scale: 1}}
			animate={{opacity: 1, scale: 1}}
			exit={{opacity: 0, scale: 0}}
			data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker-button.skin-tone-picker-button.click"
		/>
	);
});
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
				expanded={isOpen}
				data-flx="channel.emoji-picker.skin-tone-selector.skin-tone-picker-button.set-is-open"
			/>
		</div>
	);
});
