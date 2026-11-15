// SPDX-License-Identifier: AGPL-3.0-or-later

import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import styles from '@app/features/premium/components/operator_pass/OperatorPassWallQuoteField.module.css';
import type {OperatorPassWallQuoteProblem} from '@app/features/premium/utils/OperatorPassUtils';
import {CharacterCountAnnouncer} from '@app/features/ui/character_counter/CharacterCountAnnouncer';
import {Input} from '@app/features/ui/components/form/FormInput';
import {getFormattingLocale} from '@app/features/user/utils/LocaleUtils';
import {
	countOperatorPassWallQuoteCharacters,
	normalizeOperatorPassWallQuote,
	OPERATOR_PASS_WALL_QUOTE_MAX_CODE_UNITS,
	OPERATOR_PASS_WALL_QUOTE_MAX_LENGTH,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassWallQuote';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const WALL_QUOTE_LABEL_DESCRIPTOR = msg({
	message: 'Quote on the wall',
	comment:
		"Label of the optional text field for a short quote that shows with the person's listing on the Operators' wall and with their Operator badge. The wall is the list of Operators on the Operator Pass page.",
});
const WALL_QUOTE_HINT_DESCRIPTOR = msg({
	message: 'Optional. It shows in quotes on your wall listing and with your Operator badge.',
	comment:
		"Text under the \"Quote on the wall\" field in the Operators' wall settings. The wall listing is the person's entry on the Operators' wall. Operator is the badge name.",
});
const WALL_QUOTE_TOO_LONG_DESCRIPTOR = msg({
	message: 'Use 80 characters or fewer.',
	comment:
		'Error under the "Quote on the wall" field when the quote is longer than 80 characters. An emoji counts as one character.',
});
const WALL_QUOTE_LINE_BREAK_DESCRIPTOR = msg({
	message: 'Keep the quote on one line.',
	comment: 'Error under the "Quote on the wall" field when the quote has a line break.',
});
const WALL_QUOTE_CHARACTERS_DESCRIPTOR = msg({
	message: 'Remove the hidden or special characters from the quote.',
	comment:
		'Error under the "Quote on the wall" field when the quote has invisible characters, text direction controls or stacked accent marks.',
});
const WALL_QUOTE_LINK_DESCRIPTOR = msg({
	message: "The quote can't have links, web addresses, email addresses, @mentions or invites.",
	comment:
		'Error under the "Quote on the wall" field when the quote looks like a link, a web address such as example.com, an email address, an @mention or a community invite.',
});

const PROBLEM_DESCRIPTORS = {
	too_long: WALL_QUOTE_TOO_LONG_DESCRIPTOR,
	line_break: WALL_QUOTE_LINE_BREAK_DESCRIPTOR,
	characters: WALL_QUOTE_CHARACTERS_DESCRIPTOR,
	link: WALL_QUOTE_LINK_DESCRIPTOR,
} as const;

export const OperatorPassWallQuoteField = observer(function OperatorPassWallQuoteField({
	id,
	quote,
	problem,
	disabled,
	onQuoteChange,
}: {
	id: string;
	quote: string;
	problem: OperatorPassWallQuoteProblem | null;
	disabled: boolean;
	onQuoteChange: (quote: string) => void;
}) {
	const {i18n} = useLingui();
	const numberFormat = getCachedNumberFormat(getFormattingLocale());
	const length = countOperatorPassWallQuoteCharacters(normalizeOperatorPassWallQuote(quote) ?? '');
	return (
		<Input
			id={id}
			label={i18n._(WALL_QUOTE_LABEL_DESCRIPTOR)}
			labelRight={
				<>
					<span
						className={clsx(styles.count, length > OPERATOR_PASS_WALL_QUOTE_MAX_LENGTH && styles.countOver)}
						aria-hidden="true"
						data-flx="premium.operator-pass-wall-quote-field.count"
					>
						{numberFormat.format(length)}/{numberFormat.format(OPERATOR_PASS_WALL_QUOTE_MAX_LENGTH)}
					</span>
					<CharacterCountAnnouncer
						currentLength={length}
						maxLength={OPERATOR_PASS_WALL_QUOTE_MAX_LENGTH}
						data-flx="premium.operator-pass-wall-quote-field.character-count-announcer"
					/>
				</>
			}
			value={quote}
			dir="auto"
			onChange={(event) => onQuoteChange(event.target.value)}
			error={problem ? i18n._(PROBLEM_DESCRIPTORS[problem]) : null}
			footer={
				<span className={styles.hint} data-flx="premium.operator-pass-wall-quote-field.hint">
					{i18n._(WALL_QUOTE_HINT_DESCRIPTOR)}
				</span>
			}
			maxLength={OPERATOR_PASS_WALL_QUOTE_MAX_CODE_UNITS}
			autoComplete="off"
			disabled={disabled}
			data-flx="premium.operator-pass-wall-quote-field.input.quote"
		/>
	);
});
