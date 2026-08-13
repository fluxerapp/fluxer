// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/SlashCommandParamBar.module.css';
import wrapperStyles from '@app/features/channel/components/textarea/InputWrapper.module.css';
import type {ActiveSlotInfo} from '@app/features/lexical/composer/slashSlots';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {flxElementClassName} from '@app/lib/react';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {XCircleIcon} from '@phosphor-icons/react';

const OPTION_IS_REQUIRED_DESCRIPTOR = msg({
	message: 'This option is required. Please provide a value.',
	comment: 'Notice shown in the slash-command parameter bar when a required argument was left empty.',
});
const CLEAR_COMMAND_DESCRIPTOR = msg({
	message: 'Clear command',
	comment: 'Accessible label for the button that clears the slash command being composed in the message box.',
});

interface SlashCommandParamBarProps {
	activeSlot: ActiveSlotInfo;
	onClear: () => void;
}

export const SlashCommandParamBar = ({activeSlot, onClear}: SlashCommandParamBarProps) => {
	const {i18n} = useLingui();
	const showRequiredError = activeSlot.isRequiredError;
	const showDescription = !showRequiredError && activeSlot.description.length > 0;
	return (
		<flx-channel-slash-command-param-bar
			className={flxElementClassName(
				wrapperStyles.box,
				wrapperStyles.wrapperSides,
				wrapperStyles.roundedTop,
				wrapperStyles.noBottomBorder,
			)}
		>
			<flx-channel-slash-command-param-bar-body className={flxElementClassName(styles.inner)}>
				<flx-channel-slash-command-param-bar-text className={flxElementClassName(styles.text)}>
					<span className={styles.name}>{activeSlot.optionName}</span>
					{showRequiredError ? (
						<span className={styles.requiredError}>{i18n._(OPTION_IS_REQUIRED_DESCRIPTOR)}</span>
					) : showDescription ? (
						<span className={styles.description}>{activeSlot.description}</span>
					) : null}
				</flx-channel-slash-command-param-bar-text>
				<flx-channel-slash-command-param-bar-controls className={flxElementClassName(styles.controls)}>
					<FocusRing offset={-2}>
						<button
							type="button"
							className={styles.button}
							onClick={onClear}
							aria-label={i18n._(CLEAR_COMMAND_DESCRIPTOR)}
						>
							<XCircleIcon className={styles.icon} />
						</button>
					</FocusRing>
				</flx-channel-slash-command-param-bar-controls>
			</flx-channel-slash-command-param-bar-body>
			<flx-channel-slash-command-param-bar-separator className={flxElementClassName(wrapperStyles.separator)} />
		</flx-channel-slash-command-param-bar>
	);
};
