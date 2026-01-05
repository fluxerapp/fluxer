import fluxer_admin/api/instance_config
import fluxer_admin/components/ui
import fluxer_admin/web.{type Context, action}
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn render_status_card(
  config: instance_config.InstanceConfig,
) -> element.Element(a) {
  let status_color = case config.manual_review_active_now {
    True -> "bg-green-100 text-green-800 border-green-200"
    False -> "bg-amber-100 text-amber-800 border-amber-200"
  }

  let status_text = case config.manual_review_active_now {
    True -> "Manual review is currently ACTIVE"
    False -> "Manual review is currently INACTIVE"
  }

  h.div([a.class("p-4 rounded-lg border " <> status_color)], [
    h.div([a.class("flex items-center gap-2")], [
      h.span([a.class("text-lg font-semibold")], [element.text(status_text)]),
    ]),
    case config.manual_review_schedule_enabled {
      True ->
        h.p([a.class("mt-2 text-sm")], [
          element.text(
            "Schedule: "
            <> int.to_string(config.manual_review_schedule_start_hour_utc)
            <> ":00 UTC to "
            <> int.to_string(config.manual_review_schedule_end_hour_utc)
            <> ":00 UTC",
          ),
        ])
      False -> element.none()
    },
  ])
}

pub fn render_config_form(
  ctx: Context,
  config: instance_config.InstanceConfig,
) -> element.Element(a) {
  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Manual Review Settings"),
    h.p([a.class("text-sm text-neutral-600 mb-4")], [
      element.text(
        "Configure whether new registrations require manual review before the account is activated.",
      ),
    ]),
    h.form(
      [
        a.method("POST"),
        action(ctx, "/instance-config?action=update"),
        a.class("space-y-6"),
      ],
      [
        h.div([a.class("space-y-2")], [
          h.label([a.class("flex items-center gap-3 cursor-pointer")], [
            h.input([
              a.type_("checkbox"),
              a.name("manual_review_enabled"),
              a.value("true"),
              a.class("w-5 h-5 rounded border-neutral-300"),
              case config.manual_review_enabled {
                True -> a.checked(True)
                False -> a.attribute("", "")
              },
            ]),
            h.span([a.class("text-sm font-medium text-neutral-900")], [
              element.text("Enable manual review for new registrations"),
            ]),
          ]),
          h.p([a.class("text-xs text-neutral-500 ml-8")], [
            element.text(
              "When enabled, new accounts will require approval before they can use the platform.",
            ),
          ]),
        ]),
        h.div([a.class("border-t border-neutral-200 pt-6")], [
          h.label([a.class("flex items-center gap-3 cursor-pointer mb-4")], [
            h.input([
              a.type_("checkbox"),
              a.name("schedule_enabled"),
              a.value("true"),
              a.class("w-5 h-5 rounded border-neutral-300"),
              case config.manual_review_schedule_enabled {
                True -> a.checked(True)
                False -> a.attribute("", "")
              },
            ]),
            h.span([a.class("text-sm font-medium text-neutral-900")], [
              element.text("Enable schedule-based activation"),
            ]),
          ]),
          h.p([a.class("text-xs text-neutral-500 mb-4")], [
            element.text(
              "When enabled, manual review will only be active during the specified hours (UTC).",
            ),
          ]),
          h.div([a.class("grid grid-cols-2 gap-4")], [
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("start_hour"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("Start Hour (UTC)")],
              ),
              h.select(
                [
                  a.name("start_hour"),
                  a.id("start_hour"),
                  a.class(
                    "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                  ),
                ],
                list.map(list.range(0, 23), fn(hour) {
                  h.option(
                    [
                      a.value(int.to_string(hour)),
                      case
                        hour == config.manual_review_schedule_start_hour_utc
                      {
                        True -> a.selected(True)
                        False -> a.attribute("", "")
                      },
                    ],
                    int.to_string(hour) <> ":00",
                  )
                }),
              ),
            ]),
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("end_hour"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("End Hour (UTC)")],
              ),
              h.select(
                [
                  a.name("end_hour"),
                  a.id("end_hour"),
                  a.class(
                    "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                  ),
                ],
                list.map(list.range(0, 23), fn(hour) {
                  h.option(
                    [
                      a.value(int.to_string(hour)),
                      case hour == config.manual_review_schedule_end_hour_utc {
                        True -> a.selected(True)
                        False -> a.attribute("", "")
                      },
                    ],
                    int.to_string(hour) <> ":00",
                  )
                }),
              ),
            ]),
          ]),
        ]),
        h.div([a.class("border-t border-neutral-200 pt-6")], [
          h.div([a.class("space-y-4")], [
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("registration_alerts_webhook_url"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("Registration Alerts Webhook URL")],
              ),
              h.input([
                a.type_("url"),
                a.name("registration_alerts_webhook_url"),
                a.id("registration_alerts_webhook_url"),
                a.value(config.registration_alerts_webhook_url),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                ),
              ]),
              h.p([a.class("text-xs text-neutral-500 mt-1")], [
                element.text(
                  "Webhook URL for receiving alerts about new user registrations.",
                ),
              ]),
            ]),
            h.div([a.class("space-y-1")], [
              h.label(
                [
                  a.for("system_alerts_webhook_url"),
                  a.class("text-sm font-medium text-neutral-700"),
                ],
                [element.text("System Alerts Webhook URL")],
              ),
              h.input([
                a.type_("url"),
                a.name("system_alerts_webhook_url"),
                a.id("system_alerts_webhook_url"),
                a.value(config.system_alerts_webhook_url),
                a.class(
                  "w-full px-3 py-2 border border-neutral-300 rounded text-sm",
                ),
              ]),
              h.p([a.class("text-xs text-neutral-500 mt-1")], [
                element.text(
                  "Webhook URL for receiving system alerts (virus scan failures, etc.).",
                ),
              ]),
            ]),
          ]),
        ]),
        h.div([a.class("pt-4 border-t border-neutral-200")], [
          ui.button_primary("Save Configuration", "submit", []),
        ]),
      ],
    ),
  ])
}

pub fn render_snowflake_reservation_section(
  ctx: Context,
  reservations: List(instance_config.SnowflakeReservation),
  can_manage: Bool,
) -> element.Element(a) {
  let sorted_reservations =
    reservations
    |> list.sort(fn(a, b) {
      case string.compare(a.email, b.email) {
        order.Lt -> order.Lt
        order.Gt -> order.Gt
        order.Eq -> order.Eq
      }
    })

  ui.card(ui.PaddingMedium, [
    ui.heading_card_with_margin("Snowflake Reservations"),
    h.p([a.class("text-sm text-neutral-500 mb-4")], [
      element.text(
        "Reserve specific snowflake IDs for trusted testers. Every reservation maps a normalized email to a hard ID.",
      ),
    ]),
    render_snowflake_reservation_table(ctx, sorted_reservations, can_manage),
    case can_manage {
      True -> render_add_snowflake_reservation_form(ctx)
      False ->
        h.p([a.class("text-sm text-neutral-500 italic")], [
          element.text(
            "You need additional permissions to modify reservations.",
          ),
        ])
    },
  ])
}

fn render_snowflake_reservation_table(
  ctx: Context,
  reservations: List(instance_config.SnowflakeReservation),
  can_manage: Bool,
) -> element.Element(a) {
  let rows = case list.is_empty(reservations) {
    True -> [
      h.tr([], [
        h.td(
          [a.class("px-6 py-4 text-sm text-neutral-500 italic"), a.colspan(4)],
          [element.text("No reservations configured")],
        ),
      ]),
    ]
    False ->
      list.map(reservations, fn(entry) {
        render_reservation_row(ctx, entry, can_manage)
      })
  }

  ui.table_container([
    h.table([a.class("min-w-full divide-y divide-neutral-200")], [
      h.thead([a.class("bg-neutral-50")], [
        h.tr([], [
          ui.table_header_cell("Email"),
          ui.table_header_cell("Snowflake"),
          ui.table_header_cell("Updated At"),
          ui.table_header_cell("Actions"),
        ]),
      ]),
      h.tbody([a.class("bg-white divide-y divide-neutral-200")], rows),
    ]),
  ])
}

fn render_reservation_row(
  ctx: Context,
  entry: instance_config.SnowflakeReservation,
  can_manage: Bool,
) -> element.Element(a) {
  h.tr([a.class("hover:bg-neutral-50 transition-colors")], [
    h.td([a.class(ui.table_cell_class <> " text-sm text-neutral-900")], [
      element.text(entry.email),
    ]),
    h.td([a.class(ui.table_cell_class)], [element.text(entry.snowflake)]),
    h.td([a.class(ui.table_cell_class)], [
      case entry.updated_at {
        option.Some(updated) -> element.text(updated)
        option.None ->
          h.span([a.class("text-neutral-400 italic")], [element.text("—")])
      },
    ]),
    h.td([a.class(ui.table_cell_class)], [
      case can_manage {
        True -> render_reservation_action_form(ctx, entry.email)
        False ->
          h.span([a.class("text-neutral-400 italic")], [element.text("—")])
      },
    ]),
  ])
}

fn render_reservation_action_form(
  ctx: Context,
  email: String,
) -> element.Element(a) {
  h.form(
    [
      a.method("POST"),
      action(ctx, "/instance-config?action=delete-reservation"),
      a.class("flex items-center gap-2"),
    ],
    [
      h.input([a.type_("hidden"), a.name("reservation_email"), a.value(email)]),
      ui.button_danger("Remove", "submit", []),
    ],
  )
}

fn render_add_snowflake_reservation_form(ctx: Context) -> element.Element(a) {
  h.form(
    [
      a.method("POST"),
      action(ctx, "/instance-config?action=add-reservation"),
      a.class("space-y-4"),
    ],
    [
      h.div([a.class("grid grid-cols-1 gap-4 md:grid-cols-2")], [
        h.div([a.class("space-y-1")], [
          h.label(
            [
              a.for("reservation_email"),
              a.class("text-sm font-medium text-neutral-700"),
            ],
            [element.text("Email (normalized)")],
          ),
          h.input([
            a.type_("email"),
            a.name("reservation_email"),
            a.id("reservation_email"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded text-sm focus:outline-none focus:ring-1 focus:ring-blue-500",
            ),
          ]),
        ]),
        h.div([a.class("space-y-1")], [
          h.label(
            [
              a.for("reservation_snowflake"),
              a.class("text-sm font-medium text-neutral-700"),
            ],
            [element.text("Snowflake ID")],
          ),
          h.input([
            a.type_("text"),
            a.name("reservation_snowflake"),
            a.id("reservation_snowflake"),
            a.class(
              "w-full px-3 py-2 border border-neutral-300 rounded text-sm focus:outline-none focus:ring-1 focus:ring-blue-500",
            ),
          ]),
        ]),
      ]),
      h.p([a.class("text-sm text-neutral-500")], [
        element.text(
          "Use normalized email addresses (lowercase) when reserving snowflake IDs.",
        ),
      ]),
      ui.button_primary("Reserve Snowflake", "submit", []),
    ],
  )
}
