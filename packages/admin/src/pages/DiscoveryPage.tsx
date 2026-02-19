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

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {hasPermission} from '@fluxer/admin/src/AccessControlList';
import {Layout} from '@fluxer/admin/src/components/Layout';
import {Badge} from '@fluxer/admin/src/components/ui/Badge';
import {PageLayout} from '@fluxer/admin/src/components/ui/Layout/PageLayout';
import {VStack} from '@fluxer/admin/src/components/ui/Layout/VStack';
import {Table} from '@fluxer/admin/src/components/ui/Table';
import {TableBody} from '@fluxer/admin/src/components/ui/TableBody';
import {TableCell} from '@fluxer/admin/src/components/ui/TableCell';
import {TableContainer} from '@fluxer/admin/src/components/ui/TableContainer';
import {TableHeader} from '@fluxer/admin/src/components/ui/TableHeader';
import {TableHeaderCell} from '@fluxer/admin/src/components/ui/TableHeaderCell';
import {TableRow} from '@fluxer/admin/src/components/ui/TableRow';
import {Heading, Text} from '@fluxer/admin/src/components/ui/Typography';
import type {Session} from '@fluxer/admin/src/types/App';
import type {AdminConfig as Config} from '@fluxer/admin/src/types/Config';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {type DiscoveryCategory, DiscoveryCategoryLabels} from '@fluxer/constants/src/DiscoveryConstants';
import type {Flash} from '@fluxer/hono/src/Flash';
import type {UserAdminResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {DiscoveryApplicationResponse} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {Button} from '@fluxer/ui/src/components/Button';
import {Card} from '@fluxer/ui/src/components/Card';
import {CsrfInput} from '@fluxer/ui/src/components/CsrfInput';
import type {FC} from 'hono/jsx';
import type {z} from 'zod';

type Application = z.infer<typeof DiscoveryApplicationResponse>;

interface StatusTabProps {
	currentStatus: string;
	basePath: string;
}

function StatusTabs({currentStatus, basePath}: StatusTabProps) {
	const statuses = ['pending', 'approved', 'rejected', 'removed'];
	return (
		<div class="flex gap-2">
			{statuses.map((status) => {
				const isActive = currentStatus === status;
				const classes = isActive
					? 'px-4 py-2 rounded-md text-sm font-medium bg-neutral-800 text-white'
					: 'px-4 py-2 rounded-md text-sm font-medium bg-neutral-100 text-neutral-600 hover:bg-neutral-200';
				return (
					<a key={status} href={`${basePath}/discovery?status=${status}`} class={classes}>
						{status.charAt(0).toUpperCase() + status.slice(1)}
					</a>
				);
			})}
		</div>
	);
}

function getStatusBadgeVariant(status: string): 'success' | 'danger' | 'warning' | 'neutral' {
	switch (status) {
		case 'pending':
			return 'warning';
		case 'approved':
			return 'success';
		case 'rejected':
			return 'danger';
		case 'removed':
			return 'danger';
		default:
			return 'neutral';
	}
}

function getCategoryLabel(categoryId: number): string {
	return DiscoveryCategoryLabels[categoryId as DiscoveryCategory] ?? 'Unknown';
}

function formatDate(isoString: string): string {
	const date = new Date(isoString);
	return date.toLocaleDateString('en-GB', {
		day: 'numeric',
		month: 'short',
		year: 'numeric',
		hour: '2-digit',
		minute: '2-digit',
	});
}

export interface DiscoveryPageProps {
	config: Config;
	session: Session;
	currentAdmin: UserAdminResponse | undefined;
	flash: Flash | undefined;
	adminAcls: Array<string>;
	assetVersion: string;
	csrfToken: string;
	applications: Array<Application>;
	currentStatus: string;
}

export const DiscoveryPage: FC<DiscoveryPageProps> = ({
	config,
	session,
	currentAdmin,
	flash,
	adminAcls,
	assetVersion,
	csrfToken,
	applications,
	currentStatus,
}) => {
	const hasReviewPermission = hasPermission(adminAcls, AdminACLs.DISCOVERY_REVIEW);
	const hasRemovePermission = hasPermission(adminAcls, AdminACLs.DISCOVERY_REMOVE);
	const canTakeAction =
		(currentStatus === 'pending' && hasReviewPermission) || (currentStatus === 'approved' && hasRemovePermission);

	return (
		<Layout
			csrfToken={csrfToken}
			title="Discovery"
			activePage="discovery"
			config={config}
			session={session}
			currentAdmin={currentAdmin}
			flash={flash}
			assetVersion={assetVersion}
		>
			{hasReviewPermission ? (
				<PageLayout maxWidth="7xl">
					<VStack gap={6}>
						<Card padding="md">
							<VStack gap={4}>
								<Heading level={1} size="2xl">
									Discovery Management
								</Heading>
								<Text size="sm" color="muted">
									Review discovery applications and manage listed communities.
								</Text>
								<StatusTabs currentStatus={currentStatus} basePath={config.basePath} />
							</VStack>
						</Card>

						<Card padding="md">
							<VStack gap={4}>
								<Heading level={2} size="xl">
									{currentStatus.charAt(0).toUpperCase() + currentStatus.slice(1)} Applications ({applications.length})
								</Heading>

								{applications.length === 0 ? (
									<Text color="muted">No {currentStatus} applications found.</Text>
								) : (
									<TableContainer>
										<Table>
											<TableHeader>
												<TableRow>
													<TableHeaderCell>Guild ID</TableHeaderCell>
													<TableHeaderCell>Category</TableHeaderCell>
													<TableHeaderCell>Description</TableHeaderCell>
													<TableHeaderCell>Status</TableHeaderCell>
													<TableHeaderCell>Applied</TableHeaderCell>
													{currentStatus !== 'pending' && <TableHeaderCell>Reviewed</TableHeaderCell>}
													{currentStatus !== 'pending' && <TableHeaderCell>Reason</TableHeaderCell>}
													{canTakeAction && <TableHeaderCell>Actions</TableHeaderCell>}
												</TableRow>
											</TableHeader>
											<TableBody>
												{applications.map((app) => (
													<TableRow key={app.guild_id}>
														<TableCell>
															<a
																href={`${config.basePath}/guilds/${app.guild_id}`}
																class="font-mono text-blue-600 text-sm hover:underline"
															>
																{app.guild_id}
															</a>
														</TableCell>
														<TableCell>{getCategoryLabel(app.category_type)}</TableCell>
														<TableCell>
															<span class="block max-w-xs truncate" title={app.description}>
																{app.description}
															</span>
														</TableCell>
														<TableCell>
															<Badge variant={getStatusBadgeVariant(app.status)} size="sm">
																{app.status.charAt(0).toUpperCase() + app.status.slice(1)}
															</Badge>
														</TableCell>
														<TableCell>{formatDate(app.applied_at)}</TableCell>
														{currentStatus !== 'pending' && (
															<TableCell>{app.reviewed_at ? formatDate(app.reviewed_at) : '—'}</TableCell>
														)}
														{currentStatus !== 'pending' && (
															<TableCell>
																<span class="block max-w-xs truncate" title={app.review_reason ?? undefined}>
																	{app.review_reason ?? '—'}
																</span>
															</TableCell>
														)}
														{canTakeAction && (
															<TableCell>
																<div class="flex gap-2">
																	{currentStatus === 'pending' && hasReviewPermission && (
																		<>
																			<form
																				method="post"
																				action={`${config.basePath}/discovery/approve`}
																				class="inline"
																			>
																				<CsrfInput token={csrfToken} />
																				<input type="hidden" name="guild_id" value={app.guild_id} />
																				<Button type="submit" variant="primary" size="small">
																					Approve
																				</Button>
																			</form>
																			<form
																				method="post"
																				action={`${config.basePath}/discovery/reject`}
																				class="inline"
																				onclick={`return promptReason(this, 'Reject this application?')`}
																			>
																				<CsrfInput token={csrfToken} />
																				<input type="hidden" name="guild_id" value={app.guild_id} />
																				<input type="hidden" name="reason" value="" />
																				<Button type="submit" variant="danger" size="small">
																					Reject
																				</Button>
																			</form>
																		</>
																	)}
																	{currentStatus === 'approved' && hasRemovePermission && (
																		<form
																			method="post"
																			action={`${config.basePath}/discovery/remove`}
																			class="inline"
																			onclick={`return promptReason(this, 'Remove this guild from discovery?')`}
																		>
																			<CsrfInput token={csrfToken} />
																			<input type="hidden" name="guild_id" value={app.guild_id} />
																			<input type="hidden" name="reason" value="" />
																			<Button type="submit" variant="danger" size="small">
																				Remove
																			</Button>
																		</form>
																	)}
																</div>
															</TableCell>
														)}
													</TableRow>
												))}
											</TableBody>
										</Table>
									</TableContainer>
								)}
							</VStack>
						</Card>
					</VStack>
				</PageLayout>
			) : (
				<Card padding="md">
					<Heading level={1} size="2xl">
						Discovery
					</Heading>
					<Text color="muted" size="sm" class="mt-2">
						You do not have permission to view discovery applications.
					</Text>
				</Card>
			)}

			<script
				defer
				dangerouslySetInnerHTML={{
					__html: PROMPT_REASON_SCRIPT,
				}}
			/>
		</Layout>
	);
};

const PROMPT_REASON_SCRIPT = `
function promptReason(form, message) {
	var reason = prompt(message + '\\n\\nPlease provide a reason:');
	if (reason === null) return false;
	if (reason.trim() === '') {
		alert('A reason is required.');
		return false;
	}
	var reasonInput = form.querySelector('input[name="reason"]');
	if (reasonInput) {
		reasonInput.value = reason.trim();
	}
	return true;
}
`;
