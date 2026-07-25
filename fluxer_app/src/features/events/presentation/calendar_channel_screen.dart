import 'dart:async';

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:fluxer_app/core/permissions/channel_effective_permissions.dart';
import 'package:fluxer_app/core/permissions/permission.dart';
import 'package:fluxer_app/core/theme/fluxer_theme_extension.dart';
import 'package:fluxer_app/features/channels/domain/channel.dart';
import 'package:fluxer_app/features/events/domain/event.dart';
import 'package:fluxer_app/features/events/presentation/event_detail_sheet.dart';
import 'package:fluxer_app/features/events/presentation/create_event_sheet.dart';
import 'package:fluxer_app/features/events/providers/events_provider.dart';
import 'package:fluxer_app/features/ui/ui.dart';
import 'package:fluxer_app/l10n/generated/fluxer_localizations.dart';
import 'package:phosphor_flutter/phosphor_flutter.dart';

class CalendarChannelScreen extends ConsumerStatefulWidget {
  const CalendarChannelScreen({
    required this.channel,
    required this.guildId,
    super.key,
  });

  final Channel channel;
  final String guildId;

  @override
  ConsumerState<CalendarChannelScreen> createState() =>
      _CalendarChannelScreenState();
}

class _CalendarChannelScreenState
    extends ConsumerState<CalendarChannelScreen> {
  DateTime _focusedMonth = DateTime.now();
  DateTime? _selectedDay;

  @override
  void initState() {
    super.initState();
    _selectedDay = DateTime.now();
  }

  void _previousMonth() {
    setState(() {
      _focusedMonth = DateTime(
        _focusedMonth.year,
        _focusedMonth.month - 1,
      );
    });
  }

  void _nextMonth() {
    setState(() {
      _focusedMonth = DateTime(
        _focusedMonth.year,
        _focusedMonth.month + 1,
      );
    });
  }

  List<DateTime> _daysInMonth(DateTime month) {
    final first = DateTime(month.year, month.month, 1);
    final last = DateTime(month.year, month.month + 1, 0);
    // Pad to start on Monday (ISO 8601)
    final startPad = (first.weekday - 1) % 7;
    final result = <DateTime>[];
    for (int i = 0; i < startPad; i++) {
      result.add(first.subtract(Duration(days: startPad - i)));
    }
    for (int d = 1; d <= last.day; d++) {
      result.add(DateTime(month.year, month.month, d));
    }
    // Pad end to full weeks
    while (result.length % 7 != 0) {
      result.add(result.last.add(const Duration(days: 1)));
    }
    return result;
  }

  bool _isSameDay(DateTime a, DateTime b) =>
      a.year == b.year && a.month == b.month && a.day == b.day;

  bool _isToday(DateTime d) => _isSameDay(d, DateTime.now());

  @override
  Widget build(BuildContext context) {
    final l10n = FluxerLocalizations.of(context);
    final eventsAsync = ref.watch(
      channelEventsProvider(widget.channel.id),
    );
    final eventsByDate = ref.watch(
      calendarEventsByDateProvider(widget.channel.id),
    );

    final int? permissionBits =
        ref.watch(channelPermissionCacheProvider)[widget.channel.id];
    final bool canCreateEvent = permissionBits == null ||
        hasPermission(permissionBits, Permission.manageEvents) ||
        hasPermission(permissionBits, Permission.manageChannels);

    final days = _daysInMonth(_focusedMonth);
    final selectedDayKey = _selectedDay != null
        ? DateTime(
            _selectedDay!.year,
            _selectedDay!.month,
            _selectedDay!.day,
          )
        : null;
    final selectedEvents =
        selectedDayKey != null ? (eventsByDate[selectedDayKey] ?? []) : [];

    return Scaffold(
      backgroundColor: context.colors.backgroundPrimary,
      body: SafeArea(
        child: Column(
          children: [
            // ── Header ─────────────────────────────────────────
            _CalendarHeader(
              channel: widget.channel,
              canCreateEvent: canCreateEvent,
              onCreateEvent: () => _openCreateEvent(context),
            ),

            // ── Month navigation ────────────────────────────────
            Padding(
              padding:
                  const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
              child: Row(
                children: [
                  IconButton(
                    icon: PhosphorIcon(
                      PhosphorIconsRegular.caretLeft,
                      color: context.colors.textSecondary,
                      size: 18,
                    ),
                    onPressed: _previousMonth,
                  ),
                  Expanded(
                    child: Text(
                      _monthLabel(_focusedMonth),
                      style: context.textStyles.heading3.copyWith(
                        color: context.colors.textPrimary,
                      ),
                      textAlign: TextAlign.center,
                    ),
                  ),
                  IconButton(
                    icon: PhosphorIcon(
                      PhosphorIconsRegular.caretRight,
                      color: context.colors.textSecondary,
                      size: 18,
                    ),
                    onPressed: _nextMonth,
                  ),
                ],
              ),
            ),

            // ── Day-of-week labels ──────────────────────────────
            Padding(
              padding: const EdgeInsets.symmetric(horizontal: 8),
              child: Row(
                children: ['M', 'T', 'W', 'T', 'F', 'S', 'S']
                    .map(
                      (d) => Expanded(
                        child: Text(
                          d,
                          textAlign: TextAlign.center,
                          style: context.textStyles.labelSmall.copyWith(
                            color: context.colors.textTertiary,
                            fontWeight: FontWeight.w700,
                          ),
                        ),
                      ),
                    )
                    .toList(),
              ),
            ),

            const SizedBox(height: 4),

            // ── Calendar grid ───────────────────────────────────
            Padding(
              padding: const EdgeInsets.symmetric(horizontal: 8),
              child: GridView.builder(
                shrinkWrap: true,
                physics: const NeverScrollableScrollPhysics(),
                gridDelegate:
                    const SliverGridDelegateWithFixedCrossAxisCount(
                  crossAxisCount: 7,
                  childAspectRatio: 1,
                ),
                itemCount: days.length,
                itemBuilder: (context, index) {
                  final day = days[index];
                  final isCurrentMonth = day.month == _focusedMonth.month;
                  final isSelected =
                      _selectedDay != null && _isSameDay(day, _selectedDay!);
                  final isToday = _isToday(day);
                  final dayKey = DateTime(day.year, day.month, day.day);
                  final hasEvents =
                      (eventsByDate[dayKey] ?? []).isNotEmpty;

                  return GestureDetector(
                    onTap: () {
                      setState(() => _selectedDay = day);
                    },
                    child: _CalendarDayCell(
                      day: day,
                      isCurrentMonth: isCurrentMonth,
                      isSelected: isSelected,
                      isToday: isToday,
                      hasEvents: hasEvents,
                    ),
                  );
                },
              ),
            ),

            const SizedBox(height: 12),

            // ── Events list for selected day ─────────────────────
            Expanded(
              child: eventsAsync.when(
                loading: () => const Center(
                  child: CircularProgressIndicator(),
                ),
                error: (err, _) => _ErrorView(
                  onRetry: () => ref.invalidate(
                    channelEventsProvider(widget.channel.id),
                  ),
                ),
                data: (_) => selectedEvents.isEmpty
                    ? _EmptyDayView(
                        selectedDay: _selectedDay,
                        canCreate: canCreateEvent,
                        onCreateEvent: () => _openCreateEvent(context),
                      )
                    : _EventListView(
                        events: selectedEvents.cast<GuildEvent>(),
                        channelId: widget.channel.id,
                        guildId: widget.guildId,
                      ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  String _monthLabel(DateTime d) {
    const months = [
      'January', 'February', 'March', 'April', 'May', 'June',
      'July', 'August', 'September', 'October', 'November', 'December',
    ];
    return '${months[d.month - 1]} ${d.year}';
  }

  void _openCreateEvent(BuildContext context) {
    unawaited(
      showCreateEventSheet(
        context,
        channelId: widget.channel.id,
        initialDate: _selectedDay ?? DateTime.now(),
      ),
    );
  }
}

// ──────────────────────────────────────────────────────────────────────────────
// Sub-widgets
// ──────────────────────────────────────────────────────────────────────────────

class _CalendarHeader extends StatelessWidget {
  const _CalendarHeader({
    required this.channel,
    required this.canCreateEvent,
    required this.onCreateEvent,
  });

  final Channel channel;
  final bool canCreateEvent;
  final VoidCallback onCreateEvent;

  @override
  Widget build(BuildContext context) {
    return Container(
      height: 56,
      padding: const EdgeInsets.symmetric(horizontal: 16),
      decoration: BoxDecoration(
        color: context.colors.channelSidebarBackground,
        border: Border(
          bottom: BorderSide(color: context.colors.borderColor),
        ),
      ),
      child: Row(
        children: [
          PhosphorIcon(
            PhosphorIconsRegular.calendarBlank,
            color: context.colors.textSecondary,
            size: 20,
          ),
          const SizedBox(width: 8),
          Expanded(
            child: Text(
              channel.name,
              style: context.textStyles.channelName.copyWith(
                color: context.colors.textPrimary,
                fontWeight: FontWeight.w700,
              ),
              overflow: TextOverflow.ellipsis,
            ),
          ),
          if (canCreateEvent)
            IconButton(
              icon: PhosphorIcon(
                PhosphorIconsRegular.plus,
                color: context.colors.textSecondary,
                size: 20,
              ),
              onPressed: onCreateEvent,
              tooltip: 'Create Event',
            ),
        ],
      ),
    );
  }
}

class _CalendarDayCell extends StatelessWidget {
  const _CalendarDayCell({
    required this.day,
    required this.isCurrentMonth,
    required this.isSelected,
    required this.isToday,
    required this.hasEvents,
  });

  final DateTime day;
  final bool isCurrentMonth;
  final bool isSelected;
  final bool isToday;
  final bool hasEvents;

  @override
  Widget build(BuildContext context) {
    final Color bgColor = isSelected
        ? context.colors.brandExperiment
        : isToday
            ? context.colors.brandExperiment.withValues(alpha: 0.15)
            : Colors.transparent;

    final Color textColor = isSelected
        ? Colors.white
        : isCurrentMonth
            ? isToday
                ? context.colors.brandExperiment
                : context.colors.textPrimary
            : context.colors.textTertiary.withValues(alpha: 0.4);

    return Container(
      margin: const EdgeInsets.all(2),
      decoration: BoxDecoration(
        color: bgColor,
        shape: BoxShape.circle,
      ),
      child: Stack(
        alignment: Alignment.center,
        children: [
          Text(
            '${day.day}',
            style: TextStyle(
              color: textColor,
              fontSize: 13,
              fontWeight: isToday || isSelected
                  ? FontWeight.w700
                  : FontWeight.normal,
            ),
          ),
          if (hasEvents && !isSelected)
            Positioned(
              bottom: 4,
              child: Container(
                width: 4,
                height: 4,
                decoration: BoxDecoration(
                  color: isSelected
                      ? Colors.white
                      : context.colors.brandExperiment,
                  shape: BoxShape.circle,
                ),
              ),
            ),
        ],
      ),
    );
  }
}

class _EventListView extends ConsumerWidget {
  const _EventListView({
    required this.events,
    required this.channelId,
    required this.guildId,
  });

  final List<GuildEvent> events;
  final String channelId;
  final String guildId;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    return ListView.builder(
      padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 4),
      itemCount: events.length,
      itemBuilder: (context, index) {
        final event = events[index];
        return _EventListTile(
          event: event,
          channelId: channelId,
          guildId: guildId,
        );
      },
    );
  }
}

class _EventListTile extends ConsumerWidget {
  const _EventListTile({
    required this.event,
    required this.channelId,
    required this.guildId,
  });

  final GuildEvent event;
  final String channelId;
  final String guildId;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final bool isOngoing = event.isOngoing;
    final bool hasEnded = event.hasEnded;

    return Opacity(
      opacity: hasEnded ? 0.5 : 1.0,
      child: GestureDetector(
        onTap: () {
          unawaited(
            showEventDetailSheet(
              context,
              event: event,
              channelId: channelId,
              guildId: guildId,
            ),
          );
        },
        child: Container(
          margin: const EdgeInsets.only(bottom: 8),
          decoration: BoxDecoration(
            color: context.colors.backgroundSecondary,
            borderRadius: BorderRadius.circular(8),
            border: isOngoing
                ? Border.all(
                    color: context.colors.brandExperiment,
                    width: 1.5,
                  )
                : null,
          ),
          child: Row(
            children: [
              // Colored side bar indicating event status
              Container(
                width: 4,
                height: 72,
                decoration: BoxDecoration(
                  color: hasEnded
                      ? context.colors.textTertiary
                      : isOngoing
                          ? context.colors.statusGreen
                          : context.colors.brandExperiment,
                  borderRadius: const BorderRadius.only(
                    topLeft: Radius.circular(8),
                    bottomLeft: Radius.circular(8),
                  ),
                ),
              ),
              Expanded(
                child: Padding(
                  padding: const EdgeInsets.symmetric(
                    horizontal: 12,
                    vertical: 10,
                  ),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Row(
                        children: [
                          if (isOngoing) ...[
                            Container(
                              padding: const EdgeInsets.symmetric(
                                horizontal: 6,
                                vertical: 2,
                              ),
                              decoration: BoxDecoration(
                                color: context.colors.statusGreen
                                    .withValues(alpha: 0.15),
                                borderRadius: BorderRadius.circular(4),
                              ),
                              child: Text(
                                'LIVE',
                                style:
                                    context.textStyles.labelSmall.copyWith(
                                  color: context.colors.statusGreen,
                                  fontWeight: FontWeight.w800,
                                  fontSize: 10,
                                ),
                              ),
                            ),
                            const SizedBox(width: 6),
                          ],
                          Expanded(
                            child: Text(
                              event.name,
                              style:
                                  context.textStyles.channelName.copyWith(
                                color: context.colors.textPrimary,
                                fontWeight: FontWeight.w600,
                              ),
                              overflow: TextOverflow.ellipsis,
                            ),
                          ),
                        ],
                      ),
                      const SizedBox(height: 4),
                      Text(
                        _formatEventTime(event),
                        style: context.textStyles.messageBody.copyWith(
                          color: context.colors.textTertiary,
                          fontSize: 12,
                        ),
                      ),
                      if (event.locationText != null ||
                          event.locationChannelId != null) ...[
                        const SizedBox(height: 2),
                        Row(
                          children: [
                            PhosphorIcon(
                              event.locationChannelId != null
                                  ? PhosphorIconsRegular.speakerHigh
                                  : PhosphorIconsRegular.mapPin,
                              size: 12,
                              color: context.colors.textTertiary,
                            ),
                            const SizedBox(width: 4),
                            Text(
                              event.locationText ?? 'Voice Channel',
                              style:
                                  context.textStyles.messageBody.copyWith(
                                color: context.colors.textTertiary,
                                fontSize: 12,
                              ),
                            ),
                          ],
                        ),
                      ],
                    ],
                  ),
                ),
              ),
              // RSVP badge
              Padding(
                padding: const EdgeInsets.only(right: 12),
                child: _RsvpChip(
                  event: event,
                  channelId: channelId,
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  String _formatEventTime(GuildEvent event) {
    final start = event.startsAt.toLocal();
    final end = event.endsAt.toLocal();
    final startStr = _timeStr(start);
    final endStr = _timeStr(end);
    return '$startStr – $endStr';
  }

  String _timeStr(DateTime dt) {
    final h = dt.hour % 12 == 0 ? 12 : dt.hour % 12;
    final m = dt.minute.toString().padLeft(2, '0');
    final ampm = dt.hour < 12 ? 'AM' : 'PM';
    return '$h:$m $ampm';
  }
}

class _RsvpChip extends ConsumerWidget {
  const _RsvpChip({required this.event, required this.channelId});

  final GuildEvent event;
  final String channelId;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    return GestureDetector(
      onTap: () {
        unawaited(
          ref
              .read(channelEventsProvider(channelId).notifier)
              .toggleRsvp(event.id),
        );
      },
      child: AnimatedContainer(
        duration: const Duration(milliseconds: 200),
        padding: const EdgeInsets.symmetric(horizontal: 10, vertical: 6),
        decoration: BoxDecoration(
          color: event.isAttending
              ? context.colors.brandExperiment
              : context.colors.backgroundTertiary,
          borderRadius: BorderRadius.circular(16),
          border: event.isAttending
              ? null
              : Border.all(color: context.colors.borderColor),
        ),
        child: Text(
          event.isAttending ? '✓ Going' : 'Going?',
          style: TextStyle(
            color: event.isAttending
                ? Colors.white
                : context.colors.textSecondary,
            fontSize: 12,
            fontWeight: FontWeight.w600,
          ),
        ),
      ),
    );
  }
}

class _EmptyDayView extends StatelessWidget {
  const _EmptyDayView({
    required this.selectedDay,
    required this.canCreate,
    required this.onCreateEvent,
  });

  final DateTime? selectedDay;
  final bool canCreate;
  final VoidCallback onCreateEvent;

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          PhosphorIcon(
            PhosphorIconsRegular.calendarBlank,
            size: 48,
            color: context.colors.textTertiary,
          ),
          const SizedBox(height: 12),
          Text(
            'No events on this day',
            style: context.textStyles.heading4.copyWith(
              color: context.colors.textSecondary,
            ),
          ),
          const SizedBox(height: 4),
          Text(
            'Select another day or create one',
            style: context.textStyles.messageBody.copyWith(
              color: context.colors.textTertiary,
            ),
          ),
          if (canCreate) ...[
            const SizedBox(height: 20),
            FluxerFilledButton(
              onPressed: onCreateEvent,
              child: const Text('Create Event'),
            ),
          ],
        ],
      ),
    );
  }
}

class _ErrorView extends StatelessWidget {
  const _ErrorView({required this.onRetry});

  final VoidCallback onRetry;

  @override
  Widget build(BuildContext context) {
    return Center(
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Text(
            'Failed to load events',
            style: context.textStyles.heading4.copyWith(
              color: context.colors.textSecondary,
            ),
          ),
          const SizedBox(height: 12),
          FluxerFilledButton(onPressed: onRetry, child: const Text('Retry')),
        ],
      ),
    );
  }
}
