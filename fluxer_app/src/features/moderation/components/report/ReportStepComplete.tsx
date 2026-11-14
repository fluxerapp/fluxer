// SPDX-License-Identifier: AGPL-3.0-or-later

import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {getActiveInstanceProductName} from '@app/features/app/state/ActiveInstanceProductName';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const FilledCheckCircleIcon: React.FC<React.ComponentProps<typeof CheckCircleIcon>> = (props) => (
	<CheckCircleIcon
		weight="fill"
		data-flx="moderation.report.report-step-complete.filled-check-circle-icon.check-circle-icon"
		{...props}
	/>
);

interface Props {
	onStartOver: () => void;
}

const REPORT_COMPLETE_DESCRIPTION_DESCRIPTOR = msg({
	message: "Thank you for helping keep {productName} safe. We'll review your report as soon as possible.",
	comment: 'Confirmation text shown after a user submits a moderation report.',
});
export const ReportStepComplete: React.FC<Props> = observer(({onStartOver}) => {
	const {i18n} = useLingui();
	return (
		<StatusSlate
			Icon={FilledCheckCircleIcon}
			title={<Trans>Report sent</Trans>}
			description={i18n._(REPORT_COMPLETE_DESCRIPTION_DESCRIPTOR, {productName: getActiveInstanceProductName()})}
			iconStyle={{color: 'var(--status-online)'}}
			actions={[
				{
					text: <Trans>Send another report</Trans>,
					onClick: onStartOver,
					variant: 'secondary',
				},
			]}
			data-flx="moderation.report.report-step-complete.status-slate"
		/>
	);
});
