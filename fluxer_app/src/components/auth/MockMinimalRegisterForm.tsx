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

import {Trans, useLingui} from '@lingui/react/macro';
import {useMemo} from 'react';
import {ExternalLink} from '~/components/common/ExternalLink';
import inputStyles from '~/components/form/Input.module.css';
import {Button} from '~/components/uikit/Button/Button';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import {Routes} from '~/Routes';
import {getCurrentLocale} from '~/utils/LocaleUtils';
import authStyles from './AuthPageStyles.module.css';
import dobStyles from './DateOfBirthField.module.css';

type DateFieldType = 'month' | 'day' | 'year';

function getDateFieldOrder(locale: string): Array<DateFieldType> {
	const formatter = new Intl.DateTimeFormat(locale, {
		year: 'numeric',
		month: '2-digit',
		day: '2-digit',
	});

	const parts = formatter.formatToParts(new Date(2000, 0, 1));
	const order: Array<DateFieldType> = [];

	for (const part of parts) {
		if (part.type === 'month' && !order.includes('month')) {
			order.push('month');
		} else if (part.type === 'day' && !order.includes('day')) {
			order.push('day');
		} else if (part.type === 'year' && !order.includes('year')) {
			order.push('year');
		}
	}

	return order;
}

interface MockMinimalRegisterFormProps {
	submitLabel: React.ReactNode;
}

export function MockMinimalRegisterForm({submitLabel}: MockMinimalRegisterFormProps) {
	const {t} = useLingui();
	const locale = getCurrentLocale();
	const fieldOrder = useMemo(() => getDateFieldOrder(locale), [locale]);

	const dateFields: Record<DateFieldType, React.ReactElement> = {
		month: (
			<div key="month" className={dobStyles.monthField}>
				<input type="text" readOnly tabIndex={-1} placeholder={t`Month`} className={inputStyles.input} />
			</div>
		),
		day: (
			<div key="day" className={dobStyles.dayField}>
				<input type="text" readOnly tabIndex={-1} placeholder={t`Day`} className={inputStyles.input} />
			</div>
		),
		year: (
			<div key="year" className={dobStyles.yearField}>
				<input type="text" readOnly tabIndex={-1} placeholder={t`Year`} className={inputStyles.input} />
			</div>
		),
	};

	const orderedFields = fieldOrder.map((fieldType) => dateFields[fieldType]);

	return (
		<div className={authStyles.form}>
			<div className={inputStyles.fieldset}>
				<div className={inputStyles.labelContainer}>
					<span className={inputStyles.label}>
						<Trans>Display name (optional)</Trans>
					</span>
				</div>
				<div className={inputStyles.inputGroup}>
					<input
						type="text"
						readOnly
						tabIndex={-1}
						placeholder={t`What should people call you?`}
						className={inputStyles.input}
					/>
				</div>
			</div>

			<div className={dobStyles.fieldset}>
				<div className={dobStyles.labelContainer}>
					<span className={dobStyles.legend}>
						<Trans>Date of birth</Trans>
					</span>
				</div>
				<div className={dobStyles.inputsContainer}>
					<div className={dobStyles.fieldsRow}>{orderedFields}</div>
				</div>
			</div>

			<div className={authStyles.consentRow}>
				<Checkbox checked={false} onChange={() => {}} disabled>
					<span className={authStyles.consentLabel}>
						<Trans>I agree to the</Trans>{' '}
						<ExternalLink href={Routes.terms()} className={authStyles.policyLink}>
							<Trans>Terms of Service</Trans>
						</ExternalLink>{' '}
						<Trans>and</Trans>{' '}
						<ExternalLink href={Routes.privacy()} className={authStyles.policyLink}>
							<Trans>Privacy Policy</Trans>
						</ExternalLink>
					</span>
				</Checkbox>
			</div>

			<Button type="button" fitContainer disabled>
				{submitLabel}
			</Button>
		</div>
	);
}
