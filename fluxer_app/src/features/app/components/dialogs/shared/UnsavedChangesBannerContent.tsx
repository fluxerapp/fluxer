// SPDX-License-Identifier: AGPL-3.0-or-later

import {Button} from '@app/features/ui/button/Button';
import type {TabData} from '@app/features/ui/state/UnsavedChanges';
import {Trans} from '@lingui/react/macro';
import type React from 'react';

interface UnsavedChangesBannerDataFlx {
	textContainer?: string;
	text: string;
	actions: string;
	resetButton: string;
	saveButton: string;
}

interface UnsavedChangesBannerContentProps {
	tabData: TabData;
	textClassName: string;
	actionsClassName: string;
	dataFlx: UnsavedChangesBannerDataFlx;
	textContainerClassName?: string;
	defaultSaveLabel?: React.ReactNode;
}

export const UnsavedChangesBannerContent: React.FC<UnsavedChangesBannerContentProps> = ({
	tabData,
	textClassName,
	actionsClassName,
	dataFlx,
	textContainerClassName,
	defaultSaveLabel,
}) => {
	const text = (
		<div className={textClassName} data-flx={dataFlx.text}>
			{tabData.bannerText ?? <Trans>You have unsaved changes.</Trans>}
		</div>
	);
	return (
		<>
			{textContainerClassName ? (
				<div className={textContainerClassName} data-flx={dataFlx.textContainer}>
					{text}
				</div>
			) : (
				text
			)}
			<div className={actionsClassName} data-flx={dataFlx.actions}>
				<Button variant="secondary" small onClick={tabData.onReset} data-flx={dataFlx.resetButton}>
					{tabData.resetLabel ?? <Trans>Reset</Trans>}
				</Button>
				<Button
					small
					onClick={tabData.onSave}
					submitting={tabData.isSubmitting}
					variant={tabData.saveVariant}
					data-flx={dataFlx.saveButton}
				>
					{tabData.saveLabel ?? defaultSaveLabel ?? <Trans>Save changes</Trans>}
				</Button>
			</div>
		</>
	);
};
