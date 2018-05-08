local mixin = {
  actions = {},
}

function mixin:init()
  self.sections = {}
  self.active_sections = {}
end

function mixin.actions:section(msg, id, isstart, length)
  if isstart then
    local section = {eventid = self.eventid, id = id, start = msg.time, eventcount = -1, length = 0}
    table.insert(self.sections, section)
    self.active_sections[id] = section
  else
    section = self.active_sections[id]
    self.active_sections[id] = false
    if section then
      section.eventcount = self.eventid - section.eventid
      section.length = tonumber(length)
    end
  end
end

return mixin
